{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import Data.Text    (Text)
import GHC.Generics
import GHC.Word     (Word8)

data SyncToArchiveOptions = SyncToArchiveOptions
  { archiveUri :: Text
  , storePath  :: Text
  , threads    :: Int
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { archiveUri :: Text
  , storePath  :: Text
  , threads    :: Int
  } deriving (Eq, Show, Generic)
