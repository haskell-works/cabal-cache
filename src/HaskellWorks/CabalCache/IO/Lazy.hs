{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.CabalCache.IO.Lazy
  ( readResource
  , readFirstAvailableResource
  , resourceExists
  , firstExistingResource
  , headS3Uri
  , writeResource
  , createLocalDirectoryIfMissing
  , linkOrCopyResource
  , readHttpUri
  , removePathRecursive
  ) where

import Antiope.Core
import Antiope.S3.Lazy                  (S3Uri)
import Control.Lens
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans.Resource
import Data.Either                      (isRight)
import Data.Generics.Product.Any
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.Location (Location (..))
import HaskellWorks.CabalCache.Show

import qualified Antiope.S3.Lazy                    as AWS
import qualified Control.Concurrent                 as IO
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified Network.AWS                        as AWS
import qualified Network.AWS.S3.CopyObject          as AWS
import qualified Network.AWS.S3.HeadObject          as AWS
import qualified Network.AWS.S3.PutObject           as AWS
import qualified Network.HTTP.Client                as HTTP
import qualified Network.HTTP.Types                 as HTTP
import qualified System.Directory                   as IO
import qualified System.FilePath.Posix              as FP
import qualified System.IO                          as IO
import qualified System.IO.Error                    as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleAwsError :: MonadCatch m => m a -> m (Either AppError a)
handleAwsError f = catch (Right <$> f) $ \(e :: AWS.Error) ->
  case e of
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status 404 _) _ _ _ _)) -> return (Left (AwsAppError s))
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status 301 _) _ _ _ _)) -> return (Left (AwsAppError s))
    _                                                                      -> throwM e

handleHttpError :: (MonadCatch m, MonadIO m) => m a -> m (Either AppError a)
handleHttpError f = catch (Right <$> f) $ \(e :: HTTP.HttpException) ->
  case e of
    (HTTP.HttpExceptionRequest _ e') -> case e' of
      HTTP.StatusCodeException resp _ -> return (Left (HttpAppError (resp & HTTP.responseStatus)))
      _                               -> return (Left (GenericAppError (tshow e')))
    _                                 -> throwM e

getS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either AppError LBS.ByteString)
getS3Uri envAws s3Uri = case reslashS3Uri s3Uri of
  (AWS.S3Uri b k) -> handleAwsError $ runAws envAws $ AWS.unsafeDownload b k

readResource :: (MonadResource m, MonadCatch m) => AWS.Env -> Location -> m (Either AppError LBS.ByteString)
readResource envAws = \case
  S3 s3Uri        -> getS3Uri envAws (reslashS3Uri s3Uri)
  Local path      -> liftIO $ do
    fileExists <- IO.doesFileExist path
    if fileExists
      then Right <$> LBS.readFile path
      else pure (Left NotFound)
  HttpUri httpUri -> liftIO $ readHttpUri (reslashHttpUri httpUri)

readFirstAvailableResource :: (MonadResource m, MonadCatch m) => AWS.Env -> [Location] -> m (Either AppError (LBS.ByteString, Location))
readFirstAvailableResource _ [] = return (Left (GenericAppError "No resources specified in read"))
readFirstAvailableResource envAws (a:as) = do
  result <- readResource envAws a
  case result of
    Right lbs -> return $ Right (lbs, a)
    Left e -> if null as
      then return $ Left e
      else readFirstAvailableResource envAws as

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> Location -> m Bool
resourceExists envAws = \case
  S3 s3Uri        -> isRight <$> runResourceT (headS3Uri envAws (reslashS3Uri s3Uri))
  HttpUri httpUri -> isRight <$> headHttpUri (reslashHttpUri httpUri)
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
firstExistingResource _ [] = return Nothing
firstExistingResource envAws (a:as) = do
  exists <- resourceExists envAws a
  if exists
    then return (Just a)
    else firstExistingResource envAws as

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> m (Either AppError AWS.HeadObjectResponse)
headS3Uri envAws s3Uri = case reslashS3Uri s3Uri of
  AWS.S3Uri b k -> handleAwsError $ runAws envAws $ AWS.send $ AWS.headObject b k

uploadToS3 :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> LBS.ByteString -> m (Either AppError ())
uploadToS3 envAws s3Uri lbs = case reslashS3Uri s3Uri of
  AWS.S3Uri b k -> do
    let req = AWS.toBody lbs
    let po  = AWS.putObject b k req
    handleAwsError $ void $ runResAws envAws $ AWS.send po

reslashS3Uri :: S3Uri -> S3Uri
reslashS3Uri uri = uri & the @"objectKey" . the @1 %~ (T.replace "\\" "/")

reslashHttpUri :: Text -> Text
reslashHttpUri = T.replace "\\" "/"

writeResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> LBS.ByteString -> ExceptT AppError m ()
writeResource envAws loc lbs = ExceptT $ case loc of
  S3 s3Uri   -> uploadToS3 envAws (reslashS3Uri s3Uri) lbs
  Local path -> liftIO (LBS.writeFile path lbs) >> return (Right ())
  HttpUri _  -> return (Left (GenericAppError "HTTP PUT method not supported"))

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  S3 _        -> return ()
  Local path  -> liftIO $ IO.createDirectoryIfMissing True path
  HttpUri _   -> return ()

copyS3Uri :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> AWS.S3Uri -> AWS.S3Uri -> ExceptT AppError m ()
copyS3Uri envAws source target = case (reslashS3Uri source, reslashS3Uri target) of
  (AWS.S3Uri sourceBucket sourceObjectKey, AWS.S3Uri targetBucket targetObjectKey) -> ExceptT $ do
    responseResult <- runResourceT $
      handleAwsError $ runAws envAws $ AWS.send (AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey)
    case responseResult of
      Right response -> do
        let responseCode = response ^. AWS.corsResponseStatus
        if 200 <= responseCode && responseCode < 300
          then return (Right ())
          else do
            liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
            return (Left RetriesFailedAppError)
      Left msg -> return (Left msg)

retry :: (Show e, MonadIO m) => Int -> ExceptT e m () -> ExceptT e m ()
retry = retryWhen (const True)

retryWhen :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m () -> ExceptT e m ()
retryWhen p n f = catchError f $ \exception -> if n > 0
  then do
    liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
    liftIO $ IO.threadDelay 1000000
    if (p exception )
      then retry (n - 1) f
      else throwError exception
  else throwError exception

retryUnless :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m () -> ExceptT e m ()
retryUnless p = retryWhen (not . p)

linkOrCopyResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> Location -> ExceptT AppError m ()
linkOrCopyResource envAws source target = case source of
  S3 sourceS3Uri -> case target of
    S3 targetS3Uri -> retryUnless ((== Just 301) . appErrorStatus) 3 (copyS3Uri envAws (reslashS3Uri sourceS3Uri) (reslashS3Uri targetS3Uri))
    Local _        -> throwError "Can't copy between different file backends"
    HttpUri _      -> throwError "Link and copy unsupported for http backend"
  Local sourcePath -> case target of
    S3 _             -> throwError "Can't copy between different file backends"
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    HttpUri _      -> throwError "Link and copy unsupported for http backend"
  HttpUri _ -> throwError "HTTP PUT method not supported"

readHttpUri :: (MonadIO m, MonadCatch m) => Text -> m (Either AppError LBS.ByteString)
readHttpUri httpUri = handleHttpError $ do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> reslashHttpUri httpUri))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri :: (MonadIO m, MonadCatch m) => Text -> m (Either AppError LBS.ByteString)
headHttpUri httpUri = handleHttpError $ do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> (reslashHttpUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

removePathRecursive :: (MonadIO m, MonadCatch m) => FilePath -> m (Either AppError ())
removePathRecursive pkgStorePath = catch action handler
  where action :: MonadIO m => m (Either AppError ())
        action = Right <$> liftIO (IO.removeDirectoryRecursive pkgStorePath)
        handler :: MonadIO m => IOError -> m (Either AppError ())
        handler e = do
          CIO.hPutStrLn IO.stderr $ "Warning: Caught " <> tshow e
          return (Left (GenericAppError (tshow e)))
