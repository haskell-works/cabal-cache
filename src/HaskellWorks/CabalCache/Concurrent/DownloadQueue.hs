{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module HaskellWorks.CabalCache.Concurrent.DownloadQueue
  ( createDownloadQueue
  , anchor
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics.Product.Any
import Data.Set                  ((\\))

import qualified Control.Concurrent.STM                  as STM
import qualified Data.Map                                as M
import qualified Data.Set                                as S
import qualified Data.Text                               as T
import qualified HaskellWorks.CabalCache.Concurrent.Type as Z
import qualified HaskellWorks.CabalCache.Data.Relation   as R

anchor :: Z.PackageId -> M.Map Z.ConsumerId Z.ProviderId -> M.Map Z.ConsumerId Z.ProviderId
anchor root dependencies = M.union dependencies $ M.singleton root (mconcat (M.elems dependencies))

createDownloadQueue :: [(Z.ProviderId, Z.ConsumerId)] -> STM.STM Z.DownloadQueue
createDownloadQueue dependencies = do
  tDependencies <- STM.newTVar (R.fromList dependencies)
  tUploading    <- STM.newTVar S.empty
  return Z.DownloadQueue {..}

takeReady :: Z.DownloadQueue -> STM.STM (Maybe Z.PackageId)
takeReady Z.DownloadQueue {..} = do
  dependencies  <- STM.readTVar tDependencies
  uploading     <- STM.readTVar tUploading

  let ready = R.range dependencies \\ R.domain dependencies \\ uploading

  case S.lookupMin ready of
    Just packageId -> do
      STM.writeTVar tUploading (S.insert packageId uploading)
      return (Just packageId)
    Nothing -> return Nothing

commit :: Z.DownloadQueue -> Z.PackageId -> STM.STM ()
commit Z.DownloadQueue {..} packageId = do
  dependencies  <- STM.readTVar tDependencies
  uploading     <- STM.readTVar tUploading

  STM.writeTVar tUploading    $ S.delete packageId uploading
  STM.writeTVar tDependencies $ R.withoutRange (S.singleton packageId) dependencies

runQueue :: MonadIO m => Z.DownloadQueue -> (Z.PackageId -> m ()) -> m ()
runQueue downloadQueue@Z.DownloadQueue {..} f = do
  maybePackageId <- liftIO $ STM.atomically $ takeReady downloadQueue

  case maybePackageId of
    Just packageId -> do
      f packageId
      liftIO $ STM.atomically $ commit downloadQueue packageId
    Nothing -> return ()
