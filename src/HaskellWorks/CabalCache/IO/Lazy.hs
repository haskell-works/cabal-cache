{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HaskellWorks.CabalCache.IO.Lazy
  ( readResource,
    readResource_,
    readFirstAvailableResource,
    readFirstAvailableResource_,
    resourceExists,
    resourceExists_,
    firstExistingResource,
    headS3Uri,
    headS3Uri_,
    writeResource,
    writeResource_,
    createLocalDirectoryIfMissing,
    linkOrCopyResource,
    linkOrCopyResource_,
    readHttpUri,
    removePathRecursive,
    readHttpUri_,
    removePathRecursive_,
  ) where

import Antiope.Core                     (fromText, runAws, runResAws)
import Antiope.S3.Lazy                  (S3Uri)
import Control.Lens                     ((&), (^.), (%~))
import Control.Monad                    (void, unless)
import Control.Monad.Catch              (MonadCatch(..), MonadThrow(throwM))
import Control.Monad.Except             (MonadIO(..), MonadError(..), MonadTrans(lift))
import Control.Monad.Trans.Except       (ExceptT(..), runExceptT, except, throwE)
import Control.Monad.Trans.Resource     (MonadResource, runResourceT, MonadUnliftIO)
import Data.Either                      (isRight)
import Data.Functor.Identity            (Identity(..))
import Data.Generics.Product.Any        (HasAny(the))
import HaskellWorks.CabalCache.AppError (AppError(..), appErrorStatus)
import HaskellWorks.CabalCache.Location (Location (..))
import HaskellWorks.CabalCache.Show     (tshow)
import Network.AWS                      (HasEnv)
import Network.AWS.Data                 (ToText(..))
import Network.URI                      (URI)

import qualified Antiope.S3.Lazy                      as AWS
import qualified Control.Concurrent                   as IO
import qualified Control.Monad.Oops                   as OO
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Text                            as T
import qualified HaskellWorks.CabalCache.IO.Console   as CIO
import qualified Network.AWS                          as AWS
import qualified Network.AWS.S3.CopyObject            as AWS
import qualified Network.AWS.S3.HeadObject            as AWS
import qualified Network.AWS.S3.PutObject             as AWS
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Client.TLS              as HTTPS
import qualified Network.HTTP.Types                   as HTTP
import qualified System.Directory                     as IO
import qualified System.FilePath.Posix                as FP
import qualified System.IO                            as IO
import qualified System.IO.Error                      as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleAwsError :: MonadCatch m => m a -> m (Either AppError a)
handleAwsError f = catch (Right <$> f) $ \(e :: AWS.Error) ->
  case e of
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status _ _) _ _ _ _)) -> return (Left (AwsAppError s))
    _                                                                    -> throwM e

handleAwsError_ :: ()
  => MonadCatch m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` AppError
  => m a
  -> m a
handleAwsError_ f = catch f $ \(e :: AWS.Error) ->
  case e of
    (AWS.ServiceError (AWS.ServiceError' _ s@(HTTP.Status _ _) _ _ _ _)) -> OO.throwM $ AwsAppError s
    _                                                                    -> throwM e

handleHttpError :: (MonadCatch m, MonadIO m) => m a -> ExceptT AppError m a
handleHttpError f = catch (lift f) $ \(e :: HTTP.HttpException) ->
  case e of
    (HTTP.HttpExceptionRequest _ e') -> case e' of
      HTTP.StatusCodeException resp _ -> throwE (HttpAppError (resp & HTTP.responseStatus))
      _                               -> throwE (GenericAppError (tshow e'))
    _                                 -> liftIO $ throwM e

handleHttpError_ :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => MonadIO m
  => Monad m
  => m a
  -> m a
handleHttpError_ f = catch f $ \(e :: HTTP.HttpException) ->
  case e of
    (HTTP.HttpExceptionRequest _ e') -> case e' of
      HTTP.StatusCodeException resp _ -> OO.throwM (HttpAppError (resp & HTTP.responseStatus))
      _                               -> OO.throwM (GenericAppError (tshow e'))
    _                                 -> liftIO $ throwM e

getS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> URI -> ExceptT AppError m LBS.ByteString
getS3Uri envAws uri = do
  AWS.S3Uri b k <- except $ uriToS3Uri (reslashUri uri)
  ExceptT . handleAwsError $ runAws envAws $ AWS.unsafeDownload b k

getS3Uri_ :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => MonadResource m
  => HasEnv r
  => r
  -> URI
  -> m LBS.ByteString
getS3Uri_ envAws uri = do
  AWS.S3Uri b k <- OO.throwLeftM $ uriToS3Uri (reslashUri uri)
  handleAwsError_ $ runAws envAws $ AWS.unsafeDownload b k

uriToS3Uri :: URI -> Either AppError S3Uri
uriToS3Uri uri = case fromText @S3Uri (tshow uri) of
  Right s3Uri -> Right s3Uri
  Left msg    -> Left . GenericAppError $ "Unable to parse URI" <> tshow msg

readResource :: (MonadResource m, MonadCatch m) => AWS.Env -> Int -> Location -> m (Either AppError LBS.ByteString)
readResource envAws maxRetries = \case
  Local path -> liftIO do
    fileExists <- IO.doesFileExist path
    if fileExists
      then Right <$> LBS.readFile path
      else pure (Left NotFound)
  Uri uri -> runExceptT $ retryS3 maxRetries $ case uri ^. the @"uriScheme" of
    "s3:"     -> getS3Uri envAws (reslashUri uri)
    "http:"   -> readHttpUri (reslashUri uri)
    "https:"  -> readHttpUri (reslashUri uri)
    scheme    -> throwE (GenericAppError ("Unrecognised uri scheme: " <> T.pack scheme))

readResource_ :: ()
  => HasEnv r
  => MonadResource m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => r
  -> Int
  -> Location
  -> ExceptT (OO.Variant e) m LBS.ByteString
readResource_ envAws maxRetries = \case
  Local path -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then liftIO $ LBS.readFile path
      else OO.throwM NotFound
  Uri uri -> retryS3_ maxRetries $ case uri ^. the @"uriScheme" of
    "s3:"     -> getS3Uri_ envAws (reslashUri uri)
    "http:"   -> readHttpUri_ (reslashUri uri)
    "https:"  -> readHttpUri_ (reslashUri uri)
    scheme    -> OO.throwM (GenericAppError ("Unrecognised uri scheme: " <> T.pack scheme))

readFirstAvailableResource :: (MonadResource m, MonadCatch m) => AWS.Env -> [Location] -> Int -> m (Either AppError (LBS.ByteString, Location))
readFirstAvailableResource _ [] _ = return (Left (GenericAppError "No resources specified in read"))
readFirstAvailableResource envAws (a:as) maxRetries = do
  result <- readResource envAws maxRetries a
  case result of
    Right lbs -> return $ Right (lbs, a)
    Left e -> if null as
      then return $ Left e
      else readFirstAvailableResource envAws as maxRetries

readFirstAvailableResource_ :: ()
  => HasEnv t
  => MonadResource m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => t
  -> [Location]
  -> Int
  -> ExceptT (OO.Variant e) m (LBS.ByteString, Location)
readFirstAvailableResource_ _ [] _ = OO.throwM $ GenericAppError "No resources specified in read"
readFirstAvailableResource_ envAws (a:as) maxRetries = do
  (, a) <$> readResource_ envAws maxRetries a
    & OO.catchM @AppError \e -> do
        if null as
          then OO.throwFM (Identity e)
          else readFirstAvailableResource_ envAws as maxRetries

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> Location -> m Bool
resourceExists envAws = \case
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
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> isRight <$> runResourceT (headS3Uri envAws (reslashUri uri))
    "http:" -> isRight <$> runExceptT (headHttpUri (reslashUri uri))
    _scheme -> return False

resourceExists_ :: ()
  => MonadUnliftIO m
  => MonadCatch m
  => HasEnv r
  => r
  -> Location
  -> ExceptT (OO.Variant e) m Bool
resourceExists_ envAws = \case
  Local path -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then return True
      else do
        symbolicLinkExists <- liftIO $ safePathIsSymbolLink path
        if symbolicLinkExists
          then do
            target <- liftIO $ IO.getSymbolicLinkTarget path
            resourceExists_ envAws (Local target)
          else return False
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> OO.suspendM runResourceT $ (True <$ headS3Uri_ envAws (reslashUri uri)) & OO.catchM @AppError (pure . const False)
    "http:" ->                            (True <$ headHttpUri_      (reslashUri uri)) & OO.catchM @AppError (pure . const False)
    _scheme -> return False

firstExistingResource :: (MonadUnliftIO m, MonadCatch m, MonadIO m) => AWS.Env -> [Location] -> m (Maybe Location)
firstExistingResource _ [] = return Nothing
firstExistingResource envAws (a:as) = do
  exists <- resourceExists envAws a
  if exists
    then return (Just a)
    else firstExistingResource envAws as

headS3Uri :: (MonadResource m, MonadCatch m) => AWS.Env -> URI -> m (Either AppError AWS.HeadObjectResponse)
headS3Uri envAws uri = runExceptT do
  AWS.S3Uri b k <- except $ uriToS3Uri (reslashUri uri)
  ExceptT . handleAwsError $ runAws envAws $ AWS.send $ AWS.headObject b k

headS3Uri_ :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => MonadResource m
  => HasEnv r
  => r
  -> URI
  -> m AWS.HeadObjectResponse
headS3Uri_ envAws uri = do
  AWS.S3Uri b k <- OO.throwLeftM $ uriToS3Uri (reslashUri uri)
  handleAwsError_ $ runAws envAws $ AWS.send $ AWS.headObject b k

uploadToS3 :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> URI -> LBS.ByteString -> ExceptT AppError m ()
uploadToS3 envAws uri lbs = do
  AWS.S3Uri b k <- except $ uriToS3Uri (reslashUri uri)
  let req = AWS.toBody lbs
  let po  = AWS.putObject b k req
  ExceptT . handleAwsError $ void $ runResAws envAws $ AWS.send po

uploadToS3_ :: ()
  => e `OO.CouldBe` AppError
  => MonadCatch m
  => MonadUnliftIO m
  => HasEnv r
  => AWS.ToBody a
  => r
  -> URI
  -> a
  -> ExceptT (OO.Variant e) m ()
uploadToS3_ envAws uri lbs = do
  AWS.S3Uri b k <- OO.throwLeftM $ uriToS3Uri (reslashUri uri)
  let req = AWS.toBody lbs
  let po  = AWS.putObject b k req
  handleAwsError_ $ void $ OO.suspendM runResourceT $ runAws envAws $ AWS.send po

reslashUri :: URI -> URI
reslashUri uri = uri & the @"uriPath" %~ fmap reslashChar
  where reslashChar :: Char -> Char
        reslashChar '\\' = '/'
        reslashChar c    = c

writeResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> Int -> LBS.ByteString -> ExceptT AppError m ()
writeResource envAws loc maxRetries lbs = ExceptT $ case loc of
  Local path -> liftIO (LBS.writeFile path lbs) >> return (Right ())
  Uri uri       -> runExceptT $ retryS3 maxRetries $ case uri ^. the @"uriScheme" of
    "s3:"   -> uploadToS3 envAws (reslashUri uri) lbs
    "http:" -> throwE (GenericAppError "HTTP PUT method not supported")
    scheme  -> throwE (GenericAppError ("Unrecognised uri scheme: " <> T.pack scheme))

-- writeResource_ :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> Int -> LBS.ByteString -> ExceptT AppError m ()
writeResource_ :: ()
  => e `OO.CouldBe` AppError
  => MonadIO m
  => MonadCatch m
  => MonadUnliftIO m
  => HasEnv r
  => r
  -> Location
  -> Int
  -> LBS.ByteString
  -> ExceptT (OO.Variant e) m ()
writeResource_ envAws loc maxRetries lbs = case loc of
  Local path -> liftIO (LBS.writeFile path lbs) >> return ()
  Uri uri -> retryS3_ maxRetries $ case uri ^. the @"uriScheme" of
    "s3:"   -> uploadToS3_ envAws (reslashUri uri) lbs
    "http:" -> OO.throwM $ GenericAppError "HTTP PUT method not supported"
    scheme  -> OO.throwM $ GenericAppError ("Unrecognised uri scheme: " <> T.pack scheme)

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  Local path -> liftIO $ IO.createDirectoryIfMissing True path
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> return ()
    "http:" -> return ()
    _scheme -> return ()

copyS3Uri :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> URI -> URI -> ExceptT AppError m ()
copyS3Uri envAws source target = do
  AWS.S3Uri sourceBucket sourceObjectKey <- except $ uriToS3Uri (reslashUri source)
  AWS.S3Uri targetBucket targetObjectKey <- except $ uriToS3Uri (reslashUri target)
  ExceptT do
    responseResult <- runResourceT do
      let copyObjectRequest = AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey
      handleAwsError $ runAws envAws $ AWS.send copyObjectRequest
    case responseResult of
      Right response -> do
        let responseCode = response ^. AWS.corsResponseStatus
        if 200 <= responseCode && responseCode < 300
          then return (Right ())
          else do
            liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
            return (Left RetriesFailedAppError)
      Left msg -> return (Left msg)

copyS3Uri_ :: ()
  => HasEnv r
  => MonadUnliftIO m
  => e `OO.CouldBe` AppError
  => r
  -> URI
  -> URI
  -> ExceptT (OO.Variant e) m ()
copyS3Uri_ envAws source target = do
  AWS.S3Uri sourceBucket sourceObjectKey <- OO.throwLeftM $ uriToS3Uri (reslashUri source)
  AWS.S3Uri targetBucket targetObjectKey <- OO.throwLeftM $ uriToS3Uri (reslashUri target)
  let copyObjectRequest = AWS.copyObject targetBucket (toText sourceBucket <> "/" <> toText sourceObjectKey) targetObjectKey
  response <- OO.suspendM runResourceT (runAws envAws $ AWS.send copyObjectRequest)
  let responseCode = response ^. AWS.corsResponseStatus
  unless (200 <= responseCode && responseCode < 300) do
    liftIO $ CIO.hPutStrLn IO.stderr $ "Error in S3 copy: " <> tshow response
    OO.throwM RetriesFailedAppError

retryWhen :: (Show e, MonadIO m) => (e -> Bool) -> Int -> ExceptT e m a -> ExceptT e m a
retryWhen p n f = catchError f $ \exception -> if n > 0
  then do
    if (p exception)
      then do
        liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
        liftIO $ IO.threadDelay 1000000
        retryWhen p (n - 1) f
      else throwError exception
  else throwError exception

retryWhen_ :: ()
  => MonadIO m
  => Show x
  => e `OO.CouldBe` x
  => (x -> Bool)
  -> Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryWhen_ p n f = f
  & do OO.snatchM \exception -> do
        if n > 0
          then do
            if p exception
              then do
                liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
                liftIO $ IO.threadDelay 1000000
                retryWhen_ p (n - 1) f
              else OO.throwM exception
          else OO.throwM exception

retryUnless :: (Show x, MonadIO m) => (x -> Bool) -> Int -> ExceptT x m a -> ExceptT x m a
retryUnless p = retryWhen (not . p)

retryUnless_ :: forall x e m a. ()
  => MonadIO m
  => Show x
  => e `OO.CouldBe` x
  => (x -> Bool)
  -> Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryUnless_ p = retryWhen_ (not . p)

retryS3 :: MonadIO m => Int -> ExceptT AppError m a -> ExceptT AppError m a
retryS3 maxRetries a = do
  retryWhen retryPredicate maxRetries a
 where
  retryPredicate appe = case appErrorStatus appe of
                          Just i   -> i `elem` retryableHTTPStatuses
                          Nothing  -> False

retryS3_ :: ()
  => MonadIO m
  => e `OO.CouldBe` AppError
  => Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryS3_ maxRetries a = do
  retryWhen_ retryPredicate maxRetries a
 where
  retryPredicate appe = case appErrorStatus appe of
                          Just i   -> i `elem` retryableHTTPStatuses
                          Nothing  -> False

  -- https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ErrorCodeList
  -- https://stackoverflow.com/a/51770411/2976251
  -- another note: linode rate limiting returns 503
retryableHTTPStatuses :: [Int]
retryableHTTPStatuses = [408, 409, 425, 426, 502, 503, 504]

linkOrCopyResource :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> Location -> ExceptT AppError m ()
linkOrCopyResource envAws source target = case source of
  Local sourcePath -> case target of
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    Uri _ -> throwError "Can't copy between different file backends"
  Uri sourceUri -> case target of
    Local _targetPath -> throwError "Can't copy between different file backends"
    Uri targetUri    -> case (sourceUri ^. the @"uriScheme", targetUri ^. the @"uriScheme") of
      ("s3:", "s3:")               -> retryUnless ((== Just 301) . appErrorStatus) 3 (copyS3Uri envAws (reslashUri sourceUri) (reslashUri targetUri))
      ("http:", "http:")           -> throwError "Link and copy unsupported for http backend"
      (sourceScheme, targetScheme) -> throwError $ GenericAppError $ "Unsupported backend combination: " <> T.pack sourceScheme <> " to " <> T.pack targetScheme

-- linkOrCopyResource_ :: (MonadUnliftIO m, MonadCatch m) => AWS.Env -> Location -> Location -> ExceptT AppError m ()
linkOrCopyResource_ :: ()
  => HasEnv r
  => MonadUnliftIO m
  => e `OO.CouldBe` AppError
  => r
  -> Location
  -> Location
  -> ExceptT (OO.Variant e) m ()
linkOrCopyResource_ envAws source target = case source of
  Local sourcePath -> case target of
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    Uri _ -> OO.throwM $ GenericAppError "Can't copy between different file backends"
  Uri sourceUri -> case target of
    Local _targetPath -> OO.throwM $ GenericAppError "Can't copy between different file backends"
    Uri targetUri    -> case (sourceUri ^. the @"uriScheme", targetUri ^. the @"uriScheme") of
      ("s3:", "s3:")               -> retryUnless_ @AppError ((== Just 301) . appErrorStatus) 3 (copyS3Uri_ envAws (reslashUri sourceUri) (reslashUri targetUri))
      ("http:", "http:")           -> OO.throwM $ GenericAppError "Link and copy unsupported for http backend"
      (sourceScheme, targetScheme) -> OO.throwM $ GenericAppError $ "Unsupported backend combination: " <> T.pack sourceScheme <> " to " <> T.pack targetScheme

readHttpUri :: (MonadIO m, MonadCatch m) => URI -> ExceptT AppError m LBS.ByteString
readHttpUri httpUri = handleHttpError do
  manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> tshow (reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

readHttpUri_ :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => MonadIO m
  => URI
  -> m LBS.ByteString
readHttpUri_ httpUri = handleHttpError_ do
  manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> tshow (reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri :: (MonadIO m, MonadCatch m) => URI -> ExceptT AppError m LBS.ByteString
headHttpUri httpUri = handleHttpError do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> tshow (reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri_ :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => MonadIO m
  =>URI
  -> m LBS.ByteString
headHttpUri_ httpUri = handleHttpError_ do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> tshow (reslashUri httpUri)))
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

removePathRecursive_ :: ()
  => e `OO.CouldBe` AppError
  => MonadCatch m
  => MonadIO m
  => [Char]
  -> ExceptT (OO.Variant e) m ()
removePathRecursive_ pkgStorePath = catch action handler
  where action = liftIO (IO.removeDirectoryRecursive pkgStorePath)
        handler :: ()
          => MonadIO m
          => MonadError (OO.Variant e) m
          => e `OO.CouldBe` AppError
          => IOError
          -> m b
        handler e = do
          CIO.hPutStrLn IO.stderr $ "Warning: Caught " <> tshow e
          OO.throwM $ GenericAppError (tshow e)
