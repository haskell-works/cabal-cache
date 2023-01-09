{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.CabalCache.Error
  ( ExitFailure(..),
    CopyFailed(..),
    GenericError(..),
    InvalidUrl(..),
    NotFound(..),
    displayGenericError,
  ) where

import Data.Text    (Text)
import GHC.Generics (Generic)

data ExitFailure = ExitFailure deriving (Eq, Show, Generic)

data CopyFailed = CopyFailed deriving (Eq, Show, Generic)

data GenericError = GenericError Text deriving (Eq, Show, Generic)

data InvalidUrl = InvalidUrl
  { url    :: Text
  , reason :: Text
  } deriving (Eq, Show, Generic)

data NotFound = NotFound deriving (Eq, Show, Generic)

displayGenericError :: GenericError -> Text
displayGenericError (GenericError msg) = msg
