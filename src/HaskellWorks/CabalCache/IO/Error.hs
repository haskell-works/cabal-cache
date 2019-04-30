{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.CabalCache.IO.Error
  ( exceptFatal
  , exceptWarn
  , maybeToExcept
  , maybeToExceptM
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.Exit                        as IO
import qualified System.IO                          as IO

exceptFatal :: MonadIO m => ExceptT String m a -> ExceptT String m a
exceptFatal f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr . T.pack $ "Fatal Error: " <> e
          liftIO IO.exitFailure
          throwError e

exceptWarn :: MonadIO m => ExceptT String m a -> ExceptT String m a
exceptWarn f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr . T.pack $ "Warning: " <> e
          throwError e

maybeToExcept :: Monad m => String -> Maybe a -> ExceptT String m a
maybeToExcept message = maybe (throwError message) pure

maybeToExceptM :: Monad m => String -> m (Maybe a) -> ExceptT String m a
maybeToExceptM message = ExceptT . fmap (maybe (Left message) Right)
