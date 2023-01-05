{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.CabalCache.IO.Tar
  ( TarGroup(..),
    createTar,
    createTar_,
    extractTar,
    extractTar_,
  ) where

import Control.DeepSeq                  (NFData)
import Control.Lens                     ((^.))
import Control.Monad.Except             (ExceptT, MonadIO(..), MonadError(throwError))
import Data.Generics.Product.Any        (HasAny(the))
import GHC.Generics                     (Generic)
import HaskellWorks.CabalCache.AppError (AppError(GenericAppError))
import HaskellWorks.CabalCache.Show     (tshow)

import qualified Control.Monad.Oops as OO
import qualified System.Exit        as IO
import qualified System.Process     as IO

data TarGroup = TarGroup
  { basePath   :: FilePath
  , entryPaths :: [FilePath]
  } deriving (Show, Eq, Generic, NFData)

createTar :: MonadIO m => FilePath -> [TarGroup] -> ExceptT AppError m ()
createTar tarFile groups = do
  let args = ["-zcf", tarFile] <> foldMap tarGroupToArgs groups
  process <- liftIO $ IO.spawnProcess "tar" args
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throwError $ GenericAppError $ "Failed to create tar. Exit code: " <> tshow n

createTar_ :: ()
  => MonadIO m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` AppError
  => Foldable t
  => [Char]
  -> t TarGroup
  -> m ()
createTar_ tarFile groups = do
  let args = ["-zcf", tarFile] <> foldMap tarGroupToArgs groups
  process <- liftIO $ IO.spawnProcess "tar" args
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> OO.throwM $ GenericAppError $ "Failed to create tar. Exit code: " <> tshow n

extractTar :: MonadIO m => FilePath -> FilePath -> ExceptT AppError m ()
extractTar tarFile targetPath = do
  process <- liftIO $ IO.spawnProcess "tar" ["-C", targetPath, "-zxf", tarFile]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throwError $ GenericAppError $ "Failed to extract tar.  Exit code: " <> tshow n

extractTar_ :: ()
  => MonadIO m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` AppError
  => String
  -> String
  -> m ()
extractTar_ tarFile targetPath = do
  process <- liftIO $ IO.spawnProcess "tar" ["-C", targetPath, "-zxf", tarFile]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> OO.throwM $ GenericAppError $ "Failed to extract tar.  Exit code: " <> tshow n

tarGroupToArgs :: TarGroup -> [String]
tarGroupToArgs tarGroup = ["-C", tarGroup ^. the @"basePath"] <> tarGroup ^. the @"entryPaths"
