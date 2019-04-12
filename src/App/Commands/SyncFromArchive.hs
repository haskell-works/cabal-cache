{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive
  ) where

import Antiope.Core
import Antiope.Env
import App.Static
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.Maybe
import Data.Semigroup                       ((<>))
import HaskellWorks.Ci.Assist.Core
import HaskellWorks.Ci.Assist.PackageConfig (unTemplateConfig)
import HaskellWorks.Ci.Assist.Tar           (mapEntriesWith)
import Options.Applicative                  hiding (columns)

import qualified App.Commands.Options.Types     as Z
import qualified Codec.Archive.Tar              as F
import qualified Codec.Compression.GZip         as F
import qualified Data.Aeson                     as A
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Lazy.Char8     as LBSC
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as T
import qualified HaskellWorks.Ci.Assist.IO.Lazy as IO
import qualified HaskellWorks.Ci.Assist.Types   as Z
import qualified System.Directory               as IO
import qualified System.IO                      as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

logger :: LogLevel -> LBSC.ByteString -> IO ()
logger _ _ = return ()

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  let archiveUri = opts ^. the @"archiveUri"
  T.putStrLn $ "Archive URI: " <> archiveUri
  lbs <- LBS.readFile "dist-newstyle/cache/plan.json"
  case A.eitherDecode lbs of
    Right (planJson :: Z.PlanJson) -> do
      env <- mkEnv Oregon logger
      let archivePath = archiveUri <> "/" <> (planJson ^. the @"compilerId")
      let baseDir = homeDirectory <> "/.cabal/store"
      let storeCompilerPath = baseDir <> "/" <> (planJson ^. the @"compilerId")
      let storeCompilerLibPath = storeCompilerPath <> "/lib"

      IO.putStrLn "Creating store directories"
      IO.createDirectoryIfMissing True (T.unpack baseDir)
      IO.createDirectoryIfMissing True (T.unpack storeCompilerPath)
      IO.createDirectoryIfMissing True (T.unpack storeCompilerLibPath)
      IO.putStrLn $ T.unpack $ "Library path: " <> storeCompilerLibPath

      packages <- getPackages baseDir planJson

      forM_ packages $ \pInfo -> do
        let archiveFile = archiveUri <> "/" <> packageDir pInfo <> ".tar.gz"
        let packageStorePath = baseDir <> "/" <> packageDir pInfo
        storeDirectoryExists <- IO.doesDirectoryExist (T.unpack packageStorePath)
        arhiveFileExists <- runResourceT $ IO.resourceExists env archiveFile
        when (not storeDirectoryExists && arhiveFileExists) $ do
          runResAws env $ do
            maybeArchiveFileContents <- IO.readResource env archiveFile
            case maybeArchiveFileContents of
              Just archiveFileContents -> do
                liftIO $ T.putStrLn $ "Extracting " <> archiveFile
                let entries = F.read (F.decompress archiveFileContents)
                let entries' = case confPath pInfo of
                                  Nothing   -> entries
                                  Just conf -> mapEntriesWith (== T.unpack conf) (unTemplateConfig (T.unpack baseDir)) entries

                liftIO $ F.unpack (T.unpack baseDir) entries'
              Nothing -> do
                liftIO $ T.putStrLn $ "Archive unavilable: " <> archiveFile

    Left errorMessage -> do
      IO.putStrLn $ "ERROR: Unable to parse plan.json file: " <> errorMessage

  return ()

optsSyncFromArchive :: Parser Z.SyncFromArchiveOptions
optsSyncFromArchive = Z.SyncFromArchiveOptions
  <$> strOption
      (   long "archive-uri"
      <>  help "Archive URI to sync to"
      <>  metavar "S3_URI"
      <>  value (homeDirectory <> "/.cabal/archive")
      )

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = command "sync-from-archive"  $ flip info idm $ runSyncFromArchive <$> optsSyncFromArchive
