{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types where

import Data.Text    (Text)
import GHC.Generics
import GHC.Word     (Word8)

newtype SyncToArchiveOptions = SyncToArchiveOptions
  { archiveUri :: Text
  } deriving (Eq, Show, Generic)

newtype SyncFromArchiveOptions = SyncFromArchiveOptions
  { archiveUri :: Text
  } deriving (Eq, Show, Generic)
