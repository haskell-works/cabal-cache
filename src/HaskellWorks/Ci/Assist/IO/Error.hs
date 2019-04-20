{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Ci.Assist.IO.Error
  ( exceptFatal
  , exceptWarn
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.Text                         as T
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified System.Exit                       as IO
import qualified System.IO                         as IO

exceptFatal :: MonadIO m => String -> ExceptT String m a -> ExceptT String m a
exceptFatal message f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr . T.pack $ "Fatal Error: " <> e
          liftIO IO.exitFailure
          throwError e

exceptWarn :: MonadIO m => String -> ExceptT String m a -> ExceptT String m a
exceptWarn message f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr . T.pack $ "Warning: " <> e
          throwError e
