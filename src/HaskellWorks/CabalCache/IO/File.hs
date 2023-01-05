{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.IO.File
  ( copyDirectoryRecursive,
    copyDirectoryRecursive_,
    listMaybeDirectory,
  ) where

import Control.Monad.Except (ExceptT, MonadIO(..), MonadError(throwError))

import qualified Control.Monad.Oops                 as OO
import qualified Data.Text                          as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.Directory                   as IO
import qualified System.Exit                        as IO
import qualified System.Process                     as IO

copyDirectoryRecursive :: MonadIO m => FilePath -> FilePath -> ExceptT String m ()
copyDirectoryRecursive source target = do
  CIO.putStrLn $ "Copying recursively from " <> T.pack source <> " to " <> T.pack target
  process <- liftIO $ IO.spawnProcess "cp" ["-r", source, target]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throwError $ "cp exited with " <> show n

copyDirectoryRecursive_ :: ()
  => MonadIO m
  => MonadError (OO.Variant e) m
  => e `OO.CouldBe` [Char]
  => String
  -> String
  -> m ()
copyDirectoryRecursive_ source target = do
  CIO.putStrLn $ "Copying recursively from " <> T.pack source <> " to " <> T.pack target
  process <- liftIO $ IO.spawnProcess "cp" ["-r", source, target]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> OO.throwM $ "cp exited with " <> show n

listMaybeDirectory :: MonadIO m => FilePath -> m [FilePath]
listMaybeDirectory filepath = do
  exists <- liftIO $ IO.doesDirectoryExist filepath
  if exists
    then liftIO $ IO.listDirectory filepath
    else return []
