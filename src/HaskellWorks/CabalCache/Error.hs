{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.CabalCache.Error
  ( ExitFailure(ExitFailure),
    CopyFailed(CopyFailed),
    GenericError(GenericError),
    InvalidUrl(InvalidUrl),
    NotFound(NotFound),
    NotImplemented(NotImplemented),
    UnsupportedUri(UnsupportedUri),
    displayGenericError,
  ) where

import Data.Text    (Text)
import GHC.Generics (Generic)
import Network.URI  (URI)

data ExitFailure = ExitFailure deriving (Eq, Show, Generic)

data CopyFailed = CopyFailed deriving (Eq, Show, Generic)

data GenericError = GenericError Text deriving (Eq, Show, Generic)

data InvalidUrl = InvalidUrl
  { url    :: Text
  , reason :: Text
  } deriving (Eq, Show, Generic)

data NotFound = NotFound deriving (Eq, Show, Generic)

data NotImplemented = NotImplemented Text deriving (Eq, Show, Generic)

data UnsupportedUri = UnsupportedUri
  { uri    :: URI
  , reason :: Text
  } deriving (Eq, Show, Generic)

displayGenericError :: GenericError -> Text
displayGenericError (GenericError msg) = msg
