module HaskellWorks.CabalCache.Store
  ( cleanupStorePath,
    cleanupStorePath_,
  ) where

import Control.Monad                    (when, void)
import Control.Monad.Catch              (MonadCatch)
import Control.Monad.IO.Class           (MonadIO(liftIO))
import Control.Monad.Trans.Except       (ExceptT)
import HaskellWorks.CabalCache.AppError (AppError)

import qualified Control.Monad.Oops              as OO
import qualified HaskellWorks.CabalCache.IO.Lazy as IO
import qualified System.Directory                as IO

cleanupStorePath_ :: ()
  => MonadIO m
  => e `OO.CouldBe` AppError
  => MonadCatch m
  => FilePath
  -> ExceptT (OO.Variant e) m ()
cleanupStorePath_ packageStorePath = do
  pathExists <- liftIO $ IO.doesPathExist packageStorePath
  when pathExists $ void $ IO.removePathRecursive_ packageStorePath

cleanupStorePath :: MonadIO m => FilePath -> m ()
cleanupStorePath packageStorePath = do
  pathExists <- liftIO $ IO.doesPathExist packageStorePath
  when pathExists $ void $ liftIO $ IO.removePathRecursive packageStorePath
