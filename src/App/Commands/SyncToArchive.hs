{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncToArchive
  ( cmdSyncToArchive
  ) where

import Antiope.Core                         (toText)
import Antiope.Env                          (LogLevel, mkEnv)
import App.Commands.Options.Parser          (optsSyncToArchive)
import App.Static                           (homeDirectory)
import Control.Lens                         hiding ((<.>))
import Control.Monad                        (unless, when)
import Control.Monad.Except
import Control.Monad.Trans.Resource         (runResourceT)
import Data.Generics.Product.Any            (the)
import Data.List                            (isSuffixOf)
import Data.Semigroup                       ((<>))
import HaskellWorks.Ci.Assist.Core          (PackageInfo (..), Presence (..), Tagged (..), getPackages, loadPlan, relativePaths, relativePaths2)
import HaskellWorks.Ci.Assist.Location      ((<.>), (</>))
import HaskellWorks.Ci.Assist.PackageConfig (templateConfig)
import HaskellWorks.Ci.Assist.Show
import HaskellWorks.Ci.Assist.Tar           (updateEntryWith)
import Options.Applicative                  hiding (columns)
import System.Directory                     (createDirectoryIfMissing, doesDirectoryExist)

import qualified App.Commands.Options.Types        as Z
import qualified Codec.Archive.Tar                 as F
import qualified Codec.Compression.GZip            as F
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Text                         as T
import qualified HaskellWorks.Ci.Assist.GhcPkg     as GhcPkg
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified HaskellWorks.Ci.Assist.IO.Error   as IO
import qualified HaskellWorks.Ci.Assist.IO.File    as IO
import qualified HaskellWorks.Ci.Assist.IO.Lazy    as IO
import qualified HaskellWorks.Ci.Assist.IO.Tar     as IO
import qualified HaskellWorks.Ci.Assist.Types      as Z
import qualified System.Directory                  as IO
import qualified System.IO                         as IO
import qualified System.IO.Temp                    as IO
import qualified UnliftIO.Async                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runSyncToArchive :: Z.SyncToArchiveOptions -> IO ()
runSyncToArchive opts = do
  let storePath   = opts ^. the @"storePath"
  let archiveUri  = opts ^. the @"archiveUri"
  let threads     = opts ^. the @"threads"

  CIO.putStrLn $ "Store path: "   <> toText storePath
  CIO.putStrLn $ "Archive URI: "  <> toText archiveUri
  CIO.putStrLn $ "Threads: "      <> tshow threads

  mbPlan <- loadPlan
  case mbPlan of
    Right planJson -> do
      let compilerId = planJson ^. the @"compilerId"
      envAws <- mkEnv (opts ^. the @"region") (\_ _ -> pure ())
      let archivePath = archiveUri </> compilerId
      IO.createLocalDirectoryIfMissing archivePath
      let baseDir = opts ^. the @"storePath"
      CIO.putStrLn "Extracting package list"

      packages <- getPackages baseDir planJson

      let storeCompilerPath           = baseDir </> T.unpack compilerId
      let storeCompilerPackageDbPath  = storeCompilerPath </> "package.db"

      storeCompilerPackageDbPathExists <- doesDirectoryExist storeCompilerPackageDbPath

      unless storeCompilerPackageDbPathExists $
        GhcPkg.init storeCompilerPackageDbPath

      CIO.putStrLn $ "Syncing " <> tshow (length packages) <> " packages"

      -- let tempPath' = "foo"
      -- IO.createDirectoryIfMissing True tempPath'
      -- tempPath <- IO.makeAbsolute tempPath'

      IO.withSystemTempDirectory "hw-ci-assist" $ \tempPath -> do
        CIO.putStrLn $ "Temp path: " <> tshow tempPath

        CIO.putStrLn "Copying package.db directory for transformation"
        let workingStoreCompilerPath = tempPath </> T.unpack compilerId
        let workingStoreCompilerPackageDbPath = tempPath </> T.unpack compilerId </> "package.db"

        runExceptT $ IO.exceptFatal "Fatal error" $ do
          liftIO $ IO.createDirectoryIfMissing True workingStoreCompilerPackageDbPath

        packageDbFiles <- IO.listDirectory storeCompilerPackageDbPath
        let confFiles = filter (isSuffixOf ".conf") packageDbFiles

        forM_ confFiles $ \confFile -> do
          stream <- LBS.readFile (storeCompilerPackageDbPath </> confFile)
          LBS.writeFile (workingStoreCompilerPackageDbPath </> confFile) (templateConfig baseDir stream)

        IO.pooledForConcurrentlyN_ (opts ^. the @"threads") packages $ \pInfo -> do
          let archiveFileBasename = packageDir pInfo <.> ".tar.gz"
          let archiveFile = archiveUri </> T.pack archiveFileBasename
          let packageStorePath = baseDir </> packageDir pInfo
          archiveFileExists <- runResourceT $ IO.resourceExists envAws archiveFile

          unless archiveFileExists $ do
            packageStorePathExists <- doesDirectoryExist packageStorePath

            when packageStorePathExists $ void $ runExceptT $ IO.exceptWarn "Warning" $ do
              let rp2 = relativePaths2 storePath tempPath pInfo
              CIO.putStrLn $ "Creating " <> toText archiveFile

              let tempArchiveFile = tempPath </> archiveFileBasename

              IO.createTar tempArchiveFile rp2

              liftIO (LBS.readFile tempArchiveFile >>= IO.writeResource envAws archiveFile)

              return ()

    Left errorMessage -> do
      CIO.hPutStrLn IO.stderr $ "ERROR: Unable to parse plan.json file: " <> T.pack errorMessage

  return ()

cmdSyncToArchive :: Mod CommandFields (IO ())
cmdSyncToArchive = command "sync-to-archive"  $ flip info idm $ runSyncToArchive <$> optsSyncToArchive
