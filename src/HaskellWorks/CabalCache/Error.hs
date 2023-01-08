{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.CabalCache.Error
  ( ExitFailure(..),
    GenericError(..),
    NotFound(..),
    displayGenericError,
  ) where

import Data.Text    (Text)
import GHC.Generics (Generic)

data ExitFailure = ExitFailure

data GenericError = GenericError Text deriving (Eq, Show, Generic)

data NotFound = NotFound deriving (Eq, Show, Generic)

displayGenericError :: GenericError -> Text
displayGenericError (GenericError msg) = msg
