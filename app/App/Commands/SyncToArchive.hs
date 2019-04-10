{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncToArchive
  ( cmdSyncToArchive
  ) where

import Antiope.Env
import App.Static
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.Semigroup               ((<>))
import HaskellWorks.Ci.Assist.Core
import Options.Applicative          hiding (columns)

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

runSyncToArchive :: Z.SyncToArchiveOptions -> IO ()
runSyncToArchive opts = do
  let archiveUri = opts ^. the @"archiveUri"
  lbs <- LBS.readFile "dist-newstyle/cache/plan.json"
  case A.eitherDecode lbs of
    Right (planJson :: Z.PlanJson) -> do
      envAws <- mkEnv Oregon logger
      let archivePath = homeDirectory <> "/.cabal/archive/" <> (planJson ^. the @"compilerId")
      IO.createDirectoryIfMissing True (T.unpack archivePath)

      forM_ (getPackages planJson) $ \pInfo -> do
        let baseDir = homeDirectory <> "/.cabal/store/"
        let archiveFile = archiveUri <> "/" <> packageDir pInfo <> ".tar.gz"
        let packageStorePath = baseDir <> packageDir pInfo
        let packageConfigPath = baseDir <> confPath pInfo
        packageStorePathExists <- IO.doesDirectoryExist (T.unpack packageStorePath)
        archiveFileExists <- runResourceT $ IO.resourceExists envAws archiveFile
        when (not archiveFileExists && packageStorePathExists) $ do
          T.putStrLn $ "Creating " <> archiveFile
          IO.writeResource envAws archiveFile . F.compress . F.write =<< F.pack (T.unpack baseDir) (relativePaths pInfo)

    Left errorMessage -> do
      IO.putStrLn $ "ERROR: Unable to parse plan.json file: " <> errorMessage

  return ()

optsSyncToArchive :: Parser Z.SyncToArchiveOptions
optsSyncToArchive = Z.SyncToArchiveOptions
  <$> strOption
      (   long "archive-uri"
      <>  help "Archive URI to sync to"
      <>  metavar "S3_URI"
      <>  value (homeDirectory <> "/.cabal/archive")
      )

cmdSyncToArchive :: Mod CommandFields (IO ())
cmdSyncToArchive = command "sync-to-archive"  $ flip info idm $ runSyncToArchive <$> optsSyncToArchive
