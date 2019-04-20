{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Assist.IO.File
  ( copyDirectoryRecursive
  ) where

import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.Text                         as T
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO
import qualified System.Exit                       as IO
import qualified System.IO                         as IO
import qualified System.Process                    as IO

copyDirectoryRecursive :: MonadIO m => FilePath -> FilePath -> ExceptT String m ()
copyDirectoryRecursive source target = do
  CIO.putStrLn $ "Copying recursively from " <> T.pack source <> " to " <> T.pack target
  process <- liftIO $ IO.spawnProcess "cp" ["-r", source, target]
  exitCode <- liftIO $ IO.waitForProcess process
  case exitCode of
    IO.ExitSuccess   -> return ()
    IO.ExitFailure n -> throwError ""
