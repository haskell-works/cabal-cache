{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.AppError
  ( AppError(..),
    GenericError(..),
    NotFound(..),
    displayAppError,
    displayGenericError,
    appErrorStatus,
  ) where

import Data.Text                    (Text)
import GHC.Generics                 (Generic)
import HaskellWorks.CabalCache.Show (tshow)

import qualified Network.HTTP.Types as HTTP

data NotFound = NotFound deriving (Eq, Show, Generic)

data GenericError = GenericError Text deriving (Eq, Show, Generic)

data AppError
  = AwsAppError
    { status :: HTTP.Status
    }
  | HttpAppError
    { status :: HTTP.Status
    }
  | RetriesFailedAppError
  deriving (Eq, Show, Generic)

displayAppError :: AppError -> Text
displayAppError (AwsAppError s)       = tshow s
displayAppError (HttpAppError s)      = tshow s
displayAppError RetriesFailedAppError = "Multiple retries failed"

displayGenericError :: GenericError -> Text
displayGenericError (GenericError msg) = msg

appErrorStatus :: AppError -> Maybe Int
appErrorStatus (AwsAppError (HTTP.Status statusCode _)) = Just statusCode
appErrorStatus _                                        = Nothing
