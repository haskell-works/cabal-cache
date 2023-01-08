{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module HaskellWorks.CabalCache.IO.Lazy
  ( readResource,
    readFirstAvailableResource,
    resourceExists,
    writeResource,
    createLocalDirectoryIfMissing,
    linkOrCopyResource,
    readHttpUri,
    removePathRecursive,
    retryOnE,
  ) where

import Control.Lens                     ((&), (^.))
import Control.Monad                    (unless)
import Control.Monad.Catch              (MonadCatch(..), MonadThrow(throwM))
import Control.Monad.Except             (MonadIO(..), MonadError(..))
import Control.Monad.Trans.Except       (ExceptT(..))
import Control.Monad.Trans.Resource     (MonadResource, runResourceT, MonadUnliftIO)
import Data.Functor.Identity            (Identity(..))
import Data.Generics.Product.Any        (HasAny(the))
import HaskellWorks.CabalCache.AppError (AppError(..), appErrorStatus)
import HaskellWorks.CabalCache.Error    (GenericError(..), NotFound(..))
import HaskellWorks.CabalCache.Location (Location (..))
import HaskellWorks.CabalCache.Show     (tshow)
import Network.AWS                      (HasEnv)
import Network.URI                      (URI)

import qualified Control.Concurrent                   as IO
import qualified Control.Monad.Oops                   as OO
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Text                            as T
import qualified HaskellWorks.CabalCache.AWS.S3       as S3
import qualified HaskellWorks.CabalCache.IO.Console   as CIO
import qualified HaskellWorks.CabalCache.URI          as URI
import qualified Network.HTTP.Client                  as HTTP
import qualified Network.HTTP.Client.TLS              as HTTPS
import qualified System.Directory                     as IO
import qualified System.FilePath.Posix                as FP
import qualified System.IO                            as IO
import qualified System.IO.Error                      as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

handleHttpError :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => MonadIO m
  => Monad m
  => m a
  -> m a
handleHttpError f = catch f $ \(e :: HTTP.HttpException) ->
  case e of
    (HTTP.HttpExceptionRequest _ e') -> case e' of
      HTTP.StatusCodeException resp _ -> OO.throwM (HttpAppError (resp & HTTP.responseStatus))
      _                               -> OO.throwM (GenericError (tshow e'))
    _                                 -> liftIO $ throwM e

readResource :: ()
  => HasEnv r
  => MonadResource m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` NotFound
  => e `OO.CouldBe` GenericError
  => r
  -> Int
  -> Location
  -> ExceptT (OO.Variant e) m LBS.ByteString
readResource envAws maxRetries = \case
  Local path -> do
    fileExists <- liftIO $ IO.doesFileExist path
    if fileExists
      then liftIO $ LBS.readFile path
      else OO.throwM NotFound
  Uri uri -> retryS3 maxRetries $ case uri ^. the @"uriScheme" of
    "s3:"     -> S3.getS3Uri envAws (URI.reslashUri uri)
    "http:"   -> readHttpUri (URI.reslashUri uri)
    "https:"  -> readHttpUri (URI.reslashUri uri)
    scheme    -> OO.throwM (GenericError ("Unrecognised uri scheme: " <> T.pack scheme))

readFirstAvailableResource :: ()
  => HasEnv t
  => MonadResource m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` NotFound
  => e `OO.CouldBe` GenericError
  => t
  -> [Location]
  -> Int
  -> ExceptT (OO.Variant e) m (LBS.ByteString, Location)
readFirstAvailableResource _ [] _ = OO.throwM $ GenericError "No resources specified in read"
readFirstAvailableResource envAws (a:as) maxRetries = do
  (, a) <$> readResource envAws maxRetries a
    & OO.catchM @AppError \e -> do
        if null as
          then OO.throwFM (Identity e)
          else readFirstAvailableResource envAws as maxRetries

safePathIsSymbolLink :: FilePath -> IO Bool
safePathIsSymbolLink filePath = catch (IO.pathIsSymbolicLink filePath) handler
  where handler :: IOError -> IO Bool
        handler e = if IO.isDoesNotExistError e
          then return False
          else return True

resourceExists :: ()
  => MonadUnliftIO m
  => MonadCatch m
  => HasEnv r
  => r
  -> Location
  -> ExceptT (OO.Variant e) m Bool
resourceExists envAws = \case
  Local path -> do
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
    "s3:"   -> OO.suspendM runResourceT $ (True <$ S3.headS3Uri envAws (URI.reslashUri uri)) & OO.catchM @AppError (pure . const False) & OO.catchM @GenericError (pure . const False)
    "http:" ->                            (True <$ headHttpUri         (URI.reslashUri uri)) & OO.catchM @AppError (pure . const False) & OO.catchM @GenericError (pure . const False)
    _scheme -> return False

writeResource :: ()
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => MonadIO m
  => MonadCatch m
  => MonadUnliftIO m
  => HasEnv r
  => r
  -> Location
  -> Int
  -> LBS.ByteString
  -> ExceptT (OO.Variant e) m ()
writeResource envAws loc maxRetries lbs = case loc of
  Local path -> liftIO (LBS.writeFile path lbs) >> return ()
  Uri uri -> retryS3 maxRetries $ case uri ^. the @"uriScheme" of
    "s3:"   -> S3.putObject envAws (URI.reslashUri uri) lbs
    "http:" -> OO.throwM $ GenericError "HTTP PUT method not supported"
    scheme  -> OO.throwM $ GenericError ("Unrecognised uri scheme: " <> T.pack scheme)

createLocalDirectoryIfMissing :: (MonadCatch m, MonadIO m) => Location -> m ()
createLocalDirectoryIfMissing = \case
  Local path -> liftIO $ IO.createDirectoryIfMissing True path
  Uri uri       -> case uri ^. the @"uriScheme" of
    "s3:"   -> return ()
    "http:" -> return ()
    _scheme -> return ()

retryOnE :: forall e e' m a. ()
  => Monad m
  => Int
  -> ExceptT (OO.Variant e') m a
  -> ExceptT (OO.Variant (e : e')) m a
  -> ExceptT (OO.Variant e') m a
retryOnE n g f = f
  & do OO.catchM @e \_ -> if n > 0
        then retryOnE (n - 1) g f
        else g

retryWhen :: ()
  => MonadIO m
  => Show x
  => e `OO.CouldBe` x
  => (x -> Bool)
  -> Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryWhen p n f = f
  & do OO.snatchM \exception -> do
        if n > 0
          then do
            if p exception
              then do
                liftIO $ CIO.hPutStrLn IO.stderr $ "WARNING: " <> tshow exception <> " (retrying)"
                liftIO $ IO.threadDelay 1000000
                retryWhen p (n - 1) f
              else OO.throwM exception
          else OO.throwM exception

retryUnless :: forall x e m a. ()
  => MonadIO m
  => Show x
  => e `OO.CouldBe` x
  => (x -> Bool)
  -> Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryUnless p = retryWhen (not . p)

retryS3 :: ()
  => MonadIO m
  => e `OO.CouldBe` AppError
  => Int
  -> ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
retryS3 maxRetries a = do
  retryWhen retryPredicate maxRetries a
 where
  retryPredicate appe = case appErrorStatus appe of
                          Just i   -> i `elem` retryableHTTPStatuses
                          Nothing  -> False

  -- https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ErrorCodeList
  -- https://stackoverflow.com/a/51770411/2976251
  -- another note: linode rate limiting returns 503
retryableHTTPStatuses :: [Int]
retryableHTTPStatuses = [408, 409, 425, 426, 502, 503, 504]

linkOrCopyResource :: ()
  => HasEnv r
  => MonadUnliftIO m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => r
  -> Location
  -> Location
  -> ExceptT (OO.Variant e) m ()
linkOrCopyResource envAws source target = case source of
  Local sourcePath -> case target of
    Local targetPath -> do
      liftIO $ IO.createDirectoryIfMissing True (FP.takeDirectory targetPath)
      targetPathExists <- liftIO $ IO.doesFileExist targetPath
      unless targetPathExists $ liftIO $ IO.createFileLink sourcePath targetPath
    Uri _ -> OO.throwM $ GenericError "Can't copy between different file backends"
  Uri sourceUri -> case target of
    Local _targetPath -> OO.throwM $ GenericError "Can't copy between different file backends"
    Uri targetUri    -> case (sourceUri ^. the @"uriScheme", targetUri ^. the @"uriScheme") of
      ("s3:", "s3:")               -> retryUnless @AppError ((== Just 301) . appErrorStatus) 3 (S3.copyS3Uri envAws (URI.reslashUri sourceUri) (URI.reslashUri targetUri))
      ("http:", "http:")           -> OO.throwM $ GenericError "Link and copy unsupported for http backend"
      (sourceScheme, targetScheme) -> OO.throwM $ GenericError $ "Unsupported backend combination: " <> T.pack sourceScheme <> " to " <> T.pack targetScheme

readHttpUri :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => MonadIO m
  => URI
  -> m LBS.ByteString
readHttpUri httpUri = handleHttpError do
  manager <- liftIO $ HTTP.newManager HTTPS.tlsManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("GET " <> tshow (URI.reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

headHttpUri :: ()
  => MonadError (OO.Variant e) m
  => MonadCatch m
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => MonadIO m
  =>URI
  -> m LBS.ByteString
headHttpUri httpUri = handleHttpError do
  manager <- liftIO $ HTTP.newManager HTTP.defaultManagerSettings
  request <- liftIO $ HTTP.parseUrlThrow (T.unpack ("HEAD " <> tshow (URI.reslashUri httpUri)))
  response <- liftIO $ HTTP.httpLbs request manager

  return $ HTTP.responseBody response

removePathRecursive :: ()
  => e `OO.CouldBe` AppError
  => e `OO.CouldBe` GenericError
  => MonadCatch m
  => MonadIO m
  => [Char]
  -> ExceptT (OO.Variant e) m ()
removePathRecursive pkgStorePath = catch action handler
  where action = liftIO (IO.removeDirectoryRecursive pkgStorePath)
        handler :: ()
          => MonadIO m
          => MonadError (OO.Variant e) m
          => e `OO.CouldBe` GenericError
          => IOError
          -> m b
        handler e = do
          CIO.hPutStrLn IO.stderr $ "Warning: Caught " <> tshow e
          OO.throwM $ GenericError (tshow e)
