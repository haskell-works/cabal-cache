{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module HaskellWorks.CabalCache.IO.Error
  ( exceptWarn,
    exceptWarn_,
    maybeToExcept,
  ) where

import Control.Monad.Except             (ExceptT, MonadIO(..), MonadError(throwError, catchError))
import Data.Function                    ((&))
import HaskellWorks.CabalCache.AppError (displayAppError, AppError)

import qualified Control.Monad.Oops                 as OO
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.IO                          as IO

exceptWarn :: MonadIO m => ExceptT AppError m a -> ExceptT AppError m a
exceptWarn f = catchError f handler
  where handler e = do
          liftIO . CIO.hPutStrLn IO.stderr $ "Warning: " <> displayAppError e
          throwError e

exceptWarn_ :: ()
  => MonadIO m
  => e `OO.CouldBe` AppError
  => ExceptT (OO.Variant e) m a
  -> ExceptT (OO.Variant e) m a
exceptWarn_ f = f
  & do OO.snatchM @AppError \e -> do
          liftIO . CIO.hPutStrLn IO.stderr $ "Warning: " <> displayAppError e
          return (error "")

maybeToExcept :: Monad m => AppError -> Maybe a -> ExceptT AppError m a
maybeToExcept message = maybe (throwError message) pure
