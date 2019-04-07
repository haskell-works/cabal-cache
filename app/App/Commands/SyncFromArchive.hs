{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.SyncFromArchive
  ( cmdSyncFromArchive
  ) where

import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup              ((<>))
import HaskellWorks.Ci.Assist.Core
import Options.Applicative         hiding (columns)

import qualified App.Commands.Options.Types   as Z
import qualified Codec.Archive.Tar            as F
import qualified Codec.Compression.GZip       as F
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified HaskellWorks.Ci.Assist.Types as Z
import qualified System.Directory             as IO
import qualified System.IO                    as IO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runSyncFromArchive :: Z.SyncFromArchiveOptions -> IO ()
runSyncFromArchive opts = do
  lbs <- LBS.readFile "dist-newstyle/cache/plan.json"
  case A.eitherDecode lbs of
    Right (planJson :: Z.PlanJson) -> do
      home <- T.pack <$> IO.getHomeDirectory
      let archivePath = home <> "/.cabal/archive/" <> (planJson ^. the @"compilerId")

      forM_ (toPackageDirectories planJson) $ \packageDirectory -> do
        let archiveFile = home <> "/.cabal/archive/" <> packageDirectory <> ".tar.gz"
        let packageStorePath = home <> "/.cabal/store/" <> packageDirectory
        storeDirectoryExists <- IO.doesDirectoryExist (T.unpack packageStorePath)
        unless storeDirectoryExists $ do
          IO.createDirectoryIfMissing True (T.unpack packageStorePath)
          T.putStrLn $ "Extracting " <> archiveFile
          F.unpack (T.unpack packageStorePath) . F.read . F.decompress =<< LBS.readFile (T.unpack archiveFile)

    Left errorMessage -> do
      IO.putStrLn $ "ERROR: Unable to parse plan.json file: " <> errorMessage

  return ()

optsSyncFromArchive :: Parser Z.SyncFromArchiveOptions
optsSyncFromArchive = Z.SyncFromArchiveOptions
  <$> strOption
        (   long "remote-uri"
        <>  help "Remote URI to sync to"
        <>  metavar "S3_URI"
        )

cmdSyncFromArchive :: Mod CommandFields (IO ())
cmdSyncFromArchive = command "sync-from-s3"  $ flip info idm $ runSyncFromArchive <$> optsSyncFromArchive
