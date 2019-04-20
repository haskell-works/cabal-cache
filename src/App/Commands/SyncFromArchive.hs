{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive
  ) where

import Antiope.Core                         (runResAws, toText)
import Antiope.Env                          (LogLevel, mkEnv)
import App.Commands.Options.Parser          (optsSyncFromArchive)
import App.Static                           (homeDirectory)
import Control.Lens                         hiding ((<.>))
import Control.Monad                        (unless, void, when)
import Control.Monad.Except
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Trans.Resource         (runResourceT)
import Data.Generics.Product.Any            (the)
import Data.Semigroup                       ((<>))
import Data.Text                            (Text)
import HaskellWorks.Ci.Assist.Core          (PackageInfo (..), Presence (..), Tagged (..), getPackages, loadPlan)
import HaskellWorks.Ci.Assist.Location      ((<.>), (</>))
import HaskellWorks.Ci.Assist.PackageConfig (unTemplateConfig)
import HaskellWorks.Ci.Assist.Show
import HaskellWorks.Ci.Assist.Tar           (mapEntriesWith)
import Network.AWS.Types                    (Region (Oregon))
import Options.Applicative                  hiding (columns)
import System.Directory                     (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types        as Z
import qualified Codec.Archive.Tar                 as F
import qualified Codec.Compression.GZip            as F
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Text                         as T
import qualified HaskellWorks.Ci.Assist.GhcPkg     as GhcPkg
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified HaskellWorks.Ci.Assist.IO.Lazy    as IO
import qualified HaskellWorks.Ci.Assist.IO.Tar     as IO
import qualified HaskellWorks.Ci.Assist.Types      as Z
import qualified System.Directory                  as IO
import qualified System.IO                         as IO
import qualified System.IO.Temp                    as IO
import qualified UnliftIO.Async                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  let storePath   = opts ^. the @"storePath"
  let archiveUri  = opts ^. the @"archiveUri"
  let threads     = opts ^. the @"threads"

  CIO.putStrLn $ "Store path: "   <> toText storePath
  CIO.putStrLn $ "Archive URI: "  <> toText archiveUri
  CIO.putStrLn $ "Threads: "      <> tshow threads

  GhcPkg.testAvailability

  mbPlan <- loadPlan
  case mbPlan of
    Right planJson -> do
      env <- mkEnv (opts ^. the @"region") (\_ _ -> pure ())
      let compilerId                  = planJson ^. the @"compilerId"
      let archivePath                 = archiveUri </> compilerId
      let baseDir                     = opts ^. the @"storePath"
      let storeCompilerPath           = baseDir </> T.unpack compilerId
      let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"
      let storeCompilerLibPath        = storeCompilerPath </> "lib"

      CIO.putStrLn "Creating store directories"
      createDirectoryIfMissing True baseDir
      createDirectoryIfMissing True storeCompilerPath
      createDirectoryIfMissing True storeCompilerLibPath

      storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

      unless storeCompilerPackageDbPathExists $ do
        CIO.putStrLn "Package DB missing. Creating Package DB"
        GhcPkg.init storeCompilerPackageDbPath

      packages <- getPackages baseDir planJson

      IO.withSystemTempDirectory "hw-ci-assist" $ \tempPath -> do
        IO.createDirectoryIfMissing True (tempPath </> T.unpack compilerId </> "package.db")

        IO.pooledForConcurrentlyN_ threads packages $ \pInfo -> do
          let archiveBaseName = packageDir pInfo <.> ".tar.gz"
          let archiveFile = archiveUri </> T.pack archiveBaseName
          CIO.putStrLn $ "Extracting: " <> T.pack archiveBaseName
          let packageStorePath = baseDir </> packageDir pInfo
          storeDirectoryExists <- doesDirectoryExist packageStorePath
          unless storeDirectoryExists $ do
            arhiveFileExists <- runResourceT $ IO.resourceExists env archiveFile
            when arhiveFileExists $ do
              runResAws env $ do
                maybeArchiveFileContents <- IO.readResource env archiveFile

                case maybeArchiveFileContents of
                  Just archiveFileContents -> do
                    let tempArchiveFile = tempPath </> archiveBaseName :: FilePath
                    liftIO $ LBS.writeFile tempArchiveFile archiveFileContents
                    liftIO $ runExceptT $ IO.extractTar tempArchiveFile storePath

                    case confPath pInfo of
                      Tagged conf _ -> do
                        let theConfPath = storePath </> conf
                        let tempConfPath = tempPath </> conf
                        confPathExists <- liftIO $ IO.doesFileExist theConfPath
                        when confPathExists $ do
                          confContents <- liftIO $ LBS.readFile theConfPath
                          liftIO $ LBS.writeFile tempConfPath (unTemplateConfig baseDir confContents)
                          liftIO $ IO.renamePath tempConfPath theConfPath

                  Nothing -> do
                    CIO.putStrLn $ "Archive unavailable: " <> toText archiveFile

      CIO.putStrLn "Recaching package database"
      GhcPkg.recache storeCompilerPackageDbPath

    Left errorMessage -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> T.pack errorMessage

  return ()

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = command "sync-from-archive"  $ flip info idm $ runSyncFromArchive <$> optsSyncFromArchive
