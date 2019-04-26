{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HaskellWorks.Ci.Assist.IO.Lazy
  ( readResource
  , resourceExists
  , firstExistingResource
  , headS3Uri
  , writeResource
  , createLocalDirectoryIfMissing
  , linkOrCopyResource
  ) where

import Antiope.Core
import Antiope.S3.Lazy
import Control.Lens
import Control.Monad                   (void)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit.Lazy               (lazyConsume)
import Data.Either                     (isRight)
import Data.Text                       (Text)
import HaskellWorks.Ci.Assist.Location (Location (..))
import HaskellWorks.Ci.Assist.Show
import Network.AWS                     (MonadAWS, chunkedFile)
import Network.AWS.Data.Body           (_streamBody)

import qualified Antiope.S3.Lazy                   as AWS
import qualified Antiope.S3.Types                  as AWS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified Network.AWS                       as AWS
import qualified Network.AWS.Data                  as AWS
import qualified Network.AWS.S3.CopyObject         as AWS
import qualified Network.AWS.S3.HeadObject         as AWS
import qualified Network.AWS.S3.PutObject          as AWS
import qualified Network.HTTP.Types                as HTTP
import qualified System.Directory                  as IO
import qualified System.FilePath.Posix             as FP
import qualified System.IO                         as IO
import qualified System.IO.Error                   as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

readResource :: MonadResource m => AWS.Env -> Location -> m (Maybe LBS.ByteString)
readResource envAws = \case
  S3 s3Uri    -> runAws envAws $ AWS.downloadFromS3Uri s3Uri
  Local path  -> liftIO $ Just <$> LBS.readFile path

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> Location -> m Bool
resourceExists envAws = \case
  S3 s3Uri    -> isRight <$> runResourceT (headS3Uri envAws s3Uri)
  Local path  -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then return True
      else do
        symbolicLinkExists <- liftIO $ safePathIsSymbolLink path
        if symbolicLinkExists
          then do
            target <- liftIO $ IO.getSymbolicLinkTarget path
            resourceExists envAws (Local target)
          else return False

firstExistingResource :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> [Location] -> m (Maybe Location)
firstExistingResource envAws [] = return Nothing
firstExistingResource envAws (a:as) = do
  exists <- resourceExists envAws a
  if exists
    then return (Just a)
    else firstExistingResource envAws as

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either String AWS.HeadObjectResponse)
headS3Uri envAws (AWS.S3Uri b k) =
  catch (Right <$> runAws envAws (AWS.send (AWS.headObject b k))) $ \(e :: AWS.Error) ->
    case e of
      (AWS.ServiceError (AWS.ServiceError' _ (HTTP.Status 404 _) _ _ _ _)) -> return (Left "Not found")
      _                                                                    -> throwM e

chunkSize :: AWS.ChunkSize
chunkSize = AWS.ChunkSize (1024 * 1024)

uploadToS3 :: MonadUnliftIO m => AWS.Env -> AWS.S3Uri -> LBS.ByteString -> m ()
uploadToS3 envAws (AWS.S3Uri b k) lbs = do
  let req = AWS.toBody lbs
  let po  = AWS.putObject b k req
  void $ runResAws envAws $ AWS.send po

writeResource :: MonadUnliftIO m => AWS.Env -> Location -> LBS.ByteString -> m ()
writeResource envAws loc lbs = case loc of
  S3 s3Uri   -> uploadToS3 envAws s3Uri lbs
  Local path -> liftIO $ LBS.writeFile path lbs

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  S3 s3Uri   -> return ()
  Local path -> liftIO $ IO.createDirectoryIfMissing True path

copyS3Uri :: MonadUnliftIO m => AWS.Env -> AWS.S3Uri -> AWS.S3Uri -> ExceptT String m ()
copyS3Uri envAws (AWS.S3Uri sourceBucket sourceObjectKey) (AWS.S3Uri targetBucket targetObjectKey) = ExceptT $ do
  response <- runResourceT $ runAws envAws $ AWS.send (AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey)
  let responseCode = response ^. AWS.corsResponseStatus
  if 200 <= responseCode && responseCode < 300
    then return (Right ())
    else return (Left "")

linkOrCopyResource :: MonadUnliftIO m => AWS.Env -> Location -> Location -> ExceptT String m ()
linkOrCopyResource envAws source target = case source of
  S3 sourceS3Uri -> case target of
    S3 targetS3Uri -> do copyS3Uri envAws sourceS3Uri targetS3Uri
    Local _        -> throwError "Can't copy between different file backends"
  Local sourcePath -> case target of
    S3 _             -> throwError "Can't copy between different file backends"
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
