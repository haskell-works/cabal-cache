{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive,
  ) where

import App.Commands.Options.Parser      (text)
import App.Commands.Options.Types       (SyncFromArchiveOptions (SyncFromArchiveOptions))
import Control.Applicative              (optional, Alternative(..))
import Control.Lens                     ((^..), (.~), (<&>), (%~), (&), (^.), Each(each))
import Control.Lens.Combinators         (traverse1)
import Control.Monad                    (when, unless, forM_)
import Control.Monad.Catch              (MonadCatch)
import Control.Monad.Except             (ExceptT, MonadIO(..))
import Control.Monad.Trans.AWS          (envOverride, setEndpoint)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString                  (ByteString)
import Data.ByteString.Lazy.Search      (replace)
import Data.Generics.Product.Any        (the)
import Data.List.NonEmpty               (NonEmpty)
import Data.Maybe                       (fromMaybe)
import Data.Monoid                      (Dual(Dual), Endo(Endo))
import Data.Text                        (Text)
import HaskellWorks.CabalCache.AppError (AwsError, HttpError (..), displayAwsError, displayHttpError)
import HaskellWorks.CabalCache.Error    (ExitFailure(..), GenericError, InvalidUrl(..), NotFound, displayGenericError)
import HaskellWorks.CabalCache.IO.Lazy  (readFirstAvailableResource)
import HaskellWorks.CabalCache.IO.Tar   (ArchiveError(..))
import HaskellWorks.CabalCache.Location (toLocation, (<.>), (</>), Location)
import HaskellWorks.CabalCache.Metadata (loadMetadata)
import HaskellWorks.CabalCache.Show     (tshow)
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Options.Applicative              (CommandFields, Mod, Parser)
import Options.Applicative.NonEmpty     (some1)
import System.Directory                 (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types                       as Z
import qualified App.Static                                       as AS
import qualified Control.Concurrent.STM                           as STM
import qualified Control.Monad.Oops                               as OO
import qualified Data.ByteString.Char8                            as C8
import qualified Data.ByteString.Lazy                             as LBS
import qualified Data.List.NonEmpty                               as NEL
import qualified Data.Map                                         as M
import qualified Data.Map.Strict                                  as Map
import qualified Data.Text                                        as T
import qualified HaskellWorks.CabalCache.AWS.Env                  as AWS
import qualified HaskellWorks.CabalCache.Concurrent.DownloadQueue as DQ
import qualified HaskellWorks.CabalCache.Concurrent.Fork          as IO
import qualified HaskellWorks.CabalCache.Core                     as Z
import qualified HaskellWorks.CabalCache.Data.List                as L
import qualified HaskellWorks.CabalCache.GhcPkg                   as GhcPkg
import qualified HaskellWorks.CabalCache.Hash                     as H
import qualified HaskellWorks.CabalCache.IO.Console               as CIO
import qualified HaskellWorks.CabalCache.IO.Tar                   as IO
import qualified HaskellWorks.CabalCache.Store                    as M
import qualified HaskellWorks.CabalCache.Types                    as Z
import qualified Network.AWS                                      as AWS
import qualified Network.AWS.Data                                 as AWS
import qualified Options.Applicative                              as OA
import qualified System.Directory                                 as IO
import qualified System.IO                                        as IO
import qualified System.IO.Temp                                   as IO
import qualified System.IO.Unsafe                                 as IO
import Data.Semigroup (Semigroup(..))

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Reduce duplication"        -}
{- HLINT ignore "Redundant do"              -}

skippable :: Z.Package -> Bool
skippable package = package ^. the @"packageType" == "pre-existing"

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = OO.runOops $ OO.catchAndExitFailureM @ExitFailure do
  let hostEndpoint          = opts ^. the @"hostEndpoint"
  let storePath             = opts ^. the @"storePath"
  let archiveUris           = opts ^. the @"archiveUris" :: NonEmpty Location
  let threads               = opts ^. the @"threads"
  let awsLogLevel           = opts ^. the @"awsLogLevel"
  let versionedArchiveUris  = archiveUris & traverse1 %~ (</> archiveVersion) :: NonEmpty Location
  let storePathHash         = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUris     = versionedArchiveUris & traverse1 %~ (</> T.pack storePathHash)
  let maxRetries            = opts ^. the @"maxRetries"

  CIO.putStrLn $ "Store path: "       <> AWS.toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  forM_ archiveUris $ \archiveUri -> do
    CIO.putStrLn $ "Archive URI: "      <> AWS.toText archiveUri
  CIO.putStrLn $ "Archive version: "  <> archiveVersion
  CIO.putStrLn $ "Threads: "          <> tshow threads
  CIO.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

  OO.catchAndExitFailureM @ExitFailure do
    planJson <- Z.loadPlan (opts ^. the @"path" </> opts ^. the @"buildPath")
      & do OO.catchM @GenericError \e -> do
            CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> displayGenericError e
            OO.throwM ExitFailure

    compilerContext <- Z.mkCompilerContext planJson
      & do OO.catchM @Text \e -> do
            CIO.hPutStrLn IO.stderr e
            OO.throwM ExitFailure

    liftIO $ GhcPkg.testAvailability compilerContext

    envAws <- liftIO $ IO.unsafeInterleaveIO $ (<&> envOverride .~ Dual (Endo $ \s -> case hostEndpoint of
      Just (hostname, port, ssl) -> setEndpoint ssl hostname port s
      Nothing -> s))
      $ AWS.mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
    let compilerId                  = planJson ^. the @"compilerId"
    let storeCompilerPath           = storePath </> T.unpack compilerId
    let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
    let storeCompilerLibPath        = storeCompilerPath </> "lib"

    CIO.putStrLn "Creating store directories"
    liftIO $ createDirectoryIfMissing True storePath
    liftIO $ createDirectoryIfMissing True storeCompilerPath
    liftIO $ createDirectoryIfMissing True storeCompilerLibPath

    storeCompilerPackageDbPathExists <- liftIO $ doesDirectoryExist storeCompilerPackageDbPath

    unless storeCompilerPackageDbPathExists do
      CIO.putStrLn "Package DB missing. Creating Package DB"
      liftIO $ GhcPkg.init compilerContext storeCompilerPackageDbPath

    packages <- liftIO $ Z.getPackages storePath planJson

    let installPlan = planJson ^. the @"installPlan"
    let planPackages = M.fromList $ fmap (\p -> (p ^. the @"id", p)) installPlan

    let planDeps0 = installPlan >>= \p -> fmap (p ^. the @"id", ) $ mempty
          <> (p ^. the @"depends")
          <> (p ^. the @"exeDepends")
          <> (p ^.. the @"components" . each . the @"lib" . each . the @"depends"    . each)
          <> (p ^.. the @"components" . each . the @"lib" . each . the @"exeDepends" . each)
    let planDeps  = planDeps0 <> fmap (\p -> ("[universe]", p ^. the @"id")) installPlan

    downloadQueue <- liftIO $ STM.atomically $ DQ.createDownloadQueue planDeps

    let pInfos = M.fromList $ fmap (\p -> (p ^. the @"packageId", p)) packages

    IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
      liftIO $ IO.createDirectoryIfMissing True (tempPath </> T.unpack compilerId </> "package.db")

      liftIO $ IO.forkThreadsWait threads $ OO.runOops $ DQ.runQueue downloadQueue $ \packageId -> do
        OO.recoverOrVoidM @DQ.DownloadStatus do
          pInfo <- OO.throwNothingM (M.lookup packageId pInfos)
            & do OO.catchM @() \_ -> do
                  CIO.hPutStrLn IO.stderr $ "Warning: Invalid package id: " <> packageId
                  DQ.succeed

          let archiveBaseName     = Z.packageDir pInfo <.> ".tar.gz"
          let archiveFiles        = versionedArchiveUris & traverse1 %~ (</> T.pack archiveBaseName)
          let scopedArchiveFiles  = scopedArchiveUris & traverse1 %~ (</> T.pack archiveBaseName)
          let packageStorePath    = storePath </> Z.packageDir pInfo

          storeDirectoryExists <- liftIO $ doesDirectoryExist packageStorePath

          package <- OO.throwNothingM (M.lookup packageId planPackages)
            & do OO.catchM \() -> do
                  CIO.hPutStrLn IO.stderr $ "Warning: package not found" <> packageId
                  DQ.succeed

          when (skippable package) do
            CIO.putStrLn $ "Skipping: " <> packageId
            DQ.succeed

          when storeDirectoryExists DQ.succeed

          OO.suspendM runResourceT $ ensureStorePathCleanup packageStorePath do
            let locations = sconcat $ fmap L.tuple2ToNel (NEL.zip archiveFiles scopedArchiveFiles)

            (existingArchiveFileContents, existingArchiveFile) <- readFirstAvailableResource envAws locations maxRetries
              & do OO.catchM @AwsError \e -> do
                    CIO.putStrLn $ "Unable to download any of: " <> tshow locations <> " because: " <> displayAwsError e
                    DQ.fail
              & do OO.catchM @HttpError \e -> do
                    CIO.putStrLn $ "Unable to download any of: " <> tshow locations <> " because: " <> displayHttpError e
                    DQ.fail
              & do OO.catchM @NotFound \_ -> do
                    CIO.putStrLn $ "Not found: " <> tshow locations
                    DQ.fail
              & do OO.catchM @InvalidUrl \(InvalidUrl url' reason') -> do
                    CIO.hPutStrLn IO.stderr $ "Invalid URL: " <> tshow url' <> ", " <> reason'
                    DQ.fail
              & do OO.catchM @GenericError \e -> do
                    CIO.hPutStrLn IO.stderr $ displayGenericError e
                    DQ.fail

            CIO.putStrLn $ "Extracting: " <> AWS.toText existingArchiveFile

            let tempArchiveFile = tempPath </> archiveBaseName
            liftIO $ LBS.writeFile tempArchiveFile existingArchiveFileContents

            IO.extractTar tempArchiveFile storePath
              & do OO.catchM @ArchiveError \(ArchiveError reason') -> do
                    CIO.putStrLn $ "Unable to extract tar at " <> tshow tempArchiveFile <> " because: " <> reason'
                    DQ.fail

            meta <- loadMetadata packageStorePath
            oldStorePath <- OO.throwNothingM (Map.lookup "store-path" meta)
              & do OO.catchM \() -> do
                    CIO.putStrLn "store-path is missing from Metadata"
                    DQ.fail

            let Z.Tagged conf _ = Z.confPath pInfo
            
            let theConfPath = storePath </> conf
            let tempConfPath = tempPath </> conf
            confPathExists <- liftIO $ IO.doesFileExist theConfPath
            when confPathExists do
              confContents <- liftIO $ LBS.readFile theConfPath
              liftIO $ LBS.writeFile tempConfPath (replace (LBS.toStrict oldStorePath) (C8.pack storePath) confContents)
              liftIO $ IO.copyFile tempConfPath theConfPath >> IO.removeFile tempConfPath

            DQ.succeed

ensureStorePathCleanup :: ()
  => MonadIO m
  => MonadCatch m
  => e `OO.CouldBe` DQ.DownloadStatus
  => FilePath
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
ensureStorePathCleanup packageStorePath = 
  OO.snatchM @DQ.DownloadStatus \downloadStatus -> do
    case downloadStatus of
      DQ.DownloadFailure -> do
        M.cleanupStorePath packageStorePath
          & do OO.catchM @GenericError \e -> do
                CIO.hPutStrLn IO.stderr $ "Failed to cleanup store path: " <> displayGenericError e
      DQ.DownloadSuccess ->
        CIO.hPutStrLn IO.stdout $ "Successfully cleaned up store path: " <> tshow packageStorePath
    OO.throwM downloadStatus

optsSyncFromArchive :: Parser SyncFromArchiveOptions
optsSyncFromArchive = SyncFromArchiveOptions
  <$> OA.option (OA.auto <|> text)
      (  OA.long "region"
      <> OA.metavar "AWS_REGION"
      <> OA.showDefault
      <> OA.value AWS.Oregon
      <> OA.help "The AWS region in which to operate"
      )
  <*> some1
      (  OA.option (OA.maybeReader (toLocation . T.pack))
        (   OA.long "archive-uri"
        <>  OA.help "Archive URI to sync to"
        <>  OA.metavar "S3_URI"
        )
      )
  <*> OA.strOption
      (   OA.long "path"
      <>  OA.help "Path to cabal project directory.  Defaults to \".\""
      <>  OA.metavar "DIRECTORY"
      <>  OA.value AS.path
      )
  <*> OA.strOption
      (   OA.long "build-path"
      <>  OA.help ("Path to cabal build directory.  Defaults to " <> show AS.buildPath)
      <>  OA.metavar "DIRECTORY"
      <>  OA.value AS.buildPath
      )
  <*> OA.strOption
      (   OA.long "store-path"
      <>  OA.help ("Path to cabal store.  Defaults to " <> show AS.cabalStoreDirectory)
      <>  OA.metavar "DIRECTORY"
      <>  OA.value AS.cabalStoreDirectory
      )
  <*> optional
      ( OA.strOption
        (   OA.long "store-path-hash"
        <>  OA.help "Store path hash (do not use)"
        <>  OA.metavar "HASH"
        )
      )
  <*> OA.option OA.auto
      (   OA.long "threads"
      <>  OA.help "Number of concurrent threads"
      <>  OA.metavar "NUM_THREADS"
      <>  OA.value 4
      )
  <*> optional
      ( OA.option (OA.eitherReader (AWS.fromText . T.pack))
        (   OA.long "aws-log-level"
        <>  OA.help "AWS Log Level.  One of (Error, Info, Debug, Trace)"
        <>  OA.metavar "AWS_LOG_LEVEL"
        )
      )
  <*> optional parseEndpoint
  <*> OA.option OA.auto
      (   OA.long "max-retries"
      <>  OA.help "Max retries for S3 requests"
      <>  OA.metavar "NUM_RETRIES"
      <>  OA.value 3
      )

parseEndpoint :: Parser (ByteString, Int, Bool)
parseEndpoint =
  (,,)
  <$> OA.option (OA.eitherReader (AWS.fromText . T.pack))
        (   OA.long "host-name-override"
        <>  OA.help "Override the host name (default: s3.amazonaws.com)"
        <>  OA.metavar "HOST_NAME"
        )
  <*> OA.option OA.auto
        (   OA.long "host-port-override"
        <>  OA.help "Override the host port"
        <>  OA.metavar "HOST_PORT"
        )
  <*> OA.option OA.auto
        (   OA.long "host-ssl-override"
        <>  OA.help "Override the host SSL"
        <>  OA.metavar "HOST_SSL"
        )

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = OA.command "sync-from-archive" $ flip OA.info OA.idm $ runSyncFromArchive <$> optsSyncFromArchive
