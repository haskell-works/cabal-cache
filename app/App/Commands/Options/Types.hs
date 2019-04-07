{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import GHC.Generics
import GHC.Word     (Word8)

newtype SyncToArchiveOptions = SyncToArchiveOptions
  { archiveUri :: FilePath
  } deriving (Eq, Show, Generic)

newtype SyncFromArchiveOptions = SyncFromArchiveOptions
  { archiveUri  :: FilePath
  } deriving (Eq, Show, Generic)
