{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.IO.Error
  ( exceptWarn
  , maybeToExcept
  ) where

import Control.Monad.Except
import HaskellWorks.CabalCache.AppError

import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.IO                          as IO

exceptWarn :: MonadIO m => ExceptT AppError m a -> ExceptT AppError m a
exceptWarn f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr $ "Warning: " <> displayAppError e
          throwError e

maybeToExcept :: Monad m => AppError -> Maybe a -> ExceptT AppError m a
maybeToExcept message = maybe (throwError message) pure
