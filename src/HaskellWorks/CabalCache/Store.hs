module HaskellWorks.CabalCache.Store
  ( cleanupStorePath,
  ) where

import Control.Monad              (when, void)
import Control.Monad.Catch        (MonadCatch)
import Control.Monad.IO.Class     (MonadIO(liftIO))
import Control.Monad.Trans.Except (ExceptT)

import qualified Control.Monad.Oops              as OO
import qualified HaskellWorks.CabalCache.IO.Lazy as IO
import qualified System.Directory                as IO

cleanupStorePath :: ()
  => MonadIO m
  => MonadCatch m
  => FilePath
  -> ExceptT (OO.Variant e) m ()
cleanupStorePath packageStorePath = do
  pathExists <- liftIO $ IO.doesPathExist packageStorePath
  when pathExists $ void $ IO.removePathRecursive packageStorePath
