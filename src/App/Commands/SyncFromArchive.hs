{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive
  ) where

import Antiope.Core                     (runResAws, toText)
import Antiope.Env                      (LogLevel, mkEnv)
import App.Commands.Options.Parser      (optsSyncFromArchive)
import App.Static                       (homeDirectory)
import Control.Lens                     hiding ((<.>))
import Control.Monad                    (unless, void, when)
import Control.Monad.Except
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Resource     (runResourceT)
import Data.ByteString.Lazy.Search      (replace)
import Data.Generics.Product.Any        (the)
import Data.Maybe
import Data.Semigroup                   ((<>))
import Data.Text                        (Text)
import HaskellWorks.CabalCache.Core     (PackageInfo (..), Presence (..), Tagged (..), getPackages, loadPlan)
import HaskellWorks.CabalCache.IO.Error (exceptWarn, maybeToExcept, maybeToExceptM)
import HaskellWorks.CabalCache.Location ((<.>), (</>))
import HaskellWorks.CabalCache.Metadata (deleteMetadata, loadMetadata)
import HaskellWorks.CabalCache.Show
import HaskellWorks.CabalCache.Version  (archiveVersion)
import Network.AWS.Types                (Region (Oregon))
import Options.Applicative              hiding (columns)
import System.Directory                 (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types         as Z
import qualified Codec.Archive.Tar                  as F
import qualified Codec.Compression.GZip             as F
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Char8              as C8
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Map.Strict                    as Map
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.AWS.Env    as AWS
import qualified HaskellWorks.CabalCache.GhcPkg     as GhcPkg
import qualified HaskellWorks.CabalCache.Hash       as H
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified HaskellWorks.CabalCache.IO.Lazy    as IO
import qualified HaskellWorks.CabalCache.IO.Tar     as IO
import qualified HaskellWorks.CabalCache.Types      as Z
import qualified System.Directory                   as IO
import qualified System.IO                          as IO
import qualified System.IO.Temp                     as IO
import qualified UnliftIO.Async                     as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  let storePath           = opts ^. the @"storePath"
  let archiveUri          = opts ^. the @"archiveUri"
  let threads             = opts ^. the @"threads"
  let awsLogLevel         = opts ^. the @"awsLogLevel"
  let versionedArchiveUri = archiveUri </> archiveVersion
  let storePathHash       = opts ^. the @"storePathHash" & fromMaybe (H.hashStorePath storePath)
  let scopedArchiveUri    = versionedArchiveUri </> T.pack storePathHash

  CIO.putStrLn $ "Store path: "       <> toText storePath
  CIO.putStrLn $ "Store path hash: "  <> T.pack storePathHash
  CIO.putStrLn $ "Archive URI: "      <> toText archiveUri
  CIO.putStrLn $ "Archive version: "  <> archiveVersion
  CIO.putStrLn $ "Threads: "          <> tshow threads
  CIO.putStrLn $ "AWS Log level: "    <> tshow awsLogLevel

  GhcPkg.testAvailability

  mbPlan <- loadPlan
  case mbPlan of
    Right planJson -> do
      envAws <- mkEnv (opts ^. the @"region") (AWS.awsLogger awsLogLevel)
      let compilerId                  = planJson ^. the @"compilerId"
      let archivePath                 = versionedArchiveUri </> compilerId
      let storeCompilerPath           = storePath </> T.unpack compilerId
      -- xx let scopedArchivePath           = scopedArchiveUri </> compilerId
      -- xx let baseDir                     = opts ^. the @"storePath"
      -- xx let storeCompilerPath           = baseDir </> T.unpack compilerId
      let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
      let storeCompilerLibPath        = storeCompilerPath </> "lib"

      CIO.putStrLn "Creating store directories"
      createDirectoryIfMissing True storePath
      createDirectoryIfMissing True storeCompilerPath
      createDirectoryIfMissing True storeCompilerLibPath

      storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

      unless storeCompilerPackageDbPathExists $ do
        CIO.putStrLn "Package DB missing. Creating Package DB"
        GhcPkg.init storeCompilerPackageDbPath

      packages <- getPackages storePath planJson

      IO.withSystemTempDirectory "cabal-cache" $ \tempPath -> do
        IO.createDirectoryIfMissing True (tempPath </> T.unpack compilerId </> "package.db")

        IO.pooledForConcurrentlyN_ threads packages $ \pInfo -> do
          let archiveBaseName   = packageDir pInfo <.> ".tar.gz"
          let archiveFile       = versionedArchiveUri </> T.pack archiveBaseName
          let scopedArchiveFile = scopedArchiveUri </> T.pack archiveBaseName
          let packageStorePath  = storePath </> packageDir pInfo
          storeDirectoryExists <- doesDirectoryExist packageStorePath
          unless storeDirectoryExists $ do
            maybeExistingArchiveFile <- IO.firstExistingResource envAws [scopedArchiveFile, archiveFile]
            forM_ maybeExistingArchiveFile $ \existingArchiveFile -> do
              CIO.putStrLn $ "Extracting: " <> toText existingArchiveFile
              void $ runResAws envAws $ onErrorClean packageStorePath $ do
                maybeArchiveFileContents <- IO.readResource envAws existingArchiveFile

                case maybeArchiveFileContents of
                  Just archiveFileContents -> do
                    existingArchiveFileContents <- IO.readResource envAws existingArchiveFile & maybeToExceptM ("Archive unavailable: " <> show (toText archiveFile))
                    let tempArchiveFile = tempPath </> archiveBaseName
                    liftIO $ LBS.writeFile tempArchiveFile existingArchiveFileContents
                    IO.extractTar tempArchiveFile storePath

                    meta <- loadMetadata packageStorePath
                    oldStorePath <- maybeToExcept "store-path is missing from Metadata" (Map.lookup "store-path" meta)

                    case confPath pInfo of
                      Tagged conf _ -> do
                        let theConfPath = storePath </> conf
                        let tempConfPath = tempPath </> conf
                        confPathExists <- liftIO $ IO.doesFileExist theConfPath
                        when confPathExists $ do
                          confContents <- liftIO $ LBS.readFile theConfPath
                          liftIO $ LBS.writeFile tempConfPath (replace (LBS.toStrict oldStorePath) (C8.pack storePath) confContents)
                          liftIO $ IO.renamePath tempConfPath theConfPath
                  Nothing -> do
                    CIO.putStrLn $ "Archive unavailable: " <> toText existingArchiveFile

                    deleteMetadata packageStorePath

      CIO.putStrLn "Recaching package database"
      GhcPkg.recache storeCompilerPackageDbPath

    Left errorMessage -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> T.pack errorMessage

  return ()

onErrorClean :: MonadIO m => FilePath -> ExceptT String m () -> m ()
onErrorClean pkgStorePath f =
  void $ runExceptT $ exceptWarn f `catchError` (\e -> liftIO $ IO.removeDirectoryRecursive pkgStorePath)

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = command "sync-from-archive"  $ flip info idm $ runSyncFromArchive <$> optsSyncFromArchive
