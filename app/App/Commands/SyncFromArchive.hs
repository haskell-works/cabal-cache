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
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.Maybe
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

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  let archiveUri = opts ^. the @"archiveUri"
  T.putStrLn $ "Archive URI: " <> archiveUri
  lbs <- LBS.readFile "dist-newstyle/cache/plan.json"
  case A.eitherDecode lbs of
    Right (planJson :: Z.PlanJson) -> do
      env <- mkEnv Oregon logger
      let archivePath = archiveUri <> "/" <> (planJson ^. the @"compilerId")

      forM_ (toPackageDirectories planJson) $ \packageDirectory -> do
        let archiveFile = archiveUri <> "/" <> packageDirectory <> ".tar.gz"
        let packageStorePath = homeDirectory <> "/.cabal/store/" <> packageDirectory
        storeDirectoryExists <- IO.doesDirectoryExist (T.unpack packageStorePath)
        arhiveFileExists <- runResourceT $ IO.resourceExists env archiveFile
        when (not storeDirectoryExists && arhiveFileExists) $ do
          IO.createDirectoryIfMissing True (T.unpack packageStorePath)
          maybeArchiveFileContents <- runResAws env $ IO.readResource env archiveFile
          case maybeArchiveFileContents of
            Just archiveFileContents -> do
              T.putStrLn $ "Extracting " <> archiveFile
              F.unpack (T.unpack packageStorePath) (F.read (F.decompress archiveFileContents))
            Nothing -> do
              T.putStrLn $ "Archive unavilable: " <> archiveFile

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
cmdSyncFromArchive = command "sync-from-s3"  $ flip info idm $ runSyncFromArchive <$> optsSyncFromArchive
