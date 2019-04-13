{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Ci.Assist.IO.Lazy
  ( readResource
  , resourceExists
  , headS3Uri
  , writeResource
  , createLocalDirectoryIfMissing
  ) where

import Antiope.Core
import Antiope.S3.Lazy
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit.Lazy            (lazyConsume)
import Data.Text                    (Text)
import Network.AWS                  (MonadAWS, chunkedFile)
import Network.AWS.Data.Body        (_streamBody)

import qualified Antiope.S3.Lazy                   as AWS
import qualified Antiope.S3.Types                  as AWS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified Network.AWS                       as AWS
import qualified Network.AWS.Data                  as AWS
import qualified Network.AWS.S3.HeadObject         as AWS
import qualified Network.AWS.S3.PutObject          as AWS
import qualified Network.HTTP.Types                as HTTP
import qualified System.Directory                  as IO
import qualified System.IO                         as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

readResource :: MonadResource m => AWS.Env -> Text -> m (Maybe LBS.ByteString)
readResource envAws resourceUri = case AWS.fromText resourceUri of
  Right s3Uri -> runAws envAws $ AWS.downloadFromS3Uri s3Uri
  Left _      -> liftIO $ Just <$> LBS.readFile (T.unpack resourceUri)

resourceExists :: (MonadResource m, MonadCatch m, MonadIO m) => AWS.Env -> Text -> m Bool
resourceExists envAws resourceUri = case AWS.fromText resourceUri of
  Right s3Uri -> do
    result <- headS3Uri envAws s3Uri
    return (either (const False) (const True) result)
  Left x -> liftIO $ IO.doesFileExist (T.unpack resourceUri)

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either String AWS.HeadObjectResponse)
headS3Uri envAws (AWS.S3Uri b k) =
  catch (Right <$> runAws envAws (AWS.send (AWS.headObject b k))) $ \(e :: AWS.Error) ->
    case e of
      (AWS.ServiceError (AWS.ServiceError' _ (HTTP.Status 404 _) _ _ _ _)) -> return (Left "Not found")
      _                                                                    -> throwM e

chunkSize :: AWS.ChunkSize
chunkSize = AWS.ChunkSize (1024 * 1024)

uploadeToS3 :: MonadUnliftIO m => AWS.Env -> AWS.S3Uri -> LBS.ByteString -> m ()
uploadeToS3 envAws (AWS.S3Uri b k) lbs = do
  let req = AWS.toBody lbs
  let po = AWS.putObject b k req
  result <- runResAws envAws $ AWS.send po
  return ()

writeResource :: MonadUnliftIO m => AWS.Env -> Text -> LBS.ByteString -> m ()
writeResource envAws resourceUri lbs = case AWS.fromText resourceUri of
  Right s3Uri -> uploadeToS3 envAws s3Uri lbs
  Left _      -> liftIO $ LBS.writeFile (T.unpack resourceUri) lbs

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Text -> m ()
createLocalDirectoryIfMissing resourceUri = case AWS.fromText resourceUri of
  Right (s3Uri :: AWS.S3Uri) -> liftIO $ CIO.putStrLn "??" >> return ()
  Left x                     -> liftIO $ do
    CIO.putStrLn $ "ZZ: " <> resourceUri
    IO.createDirectoryIfMissing True (T.unpack resourceUri)
