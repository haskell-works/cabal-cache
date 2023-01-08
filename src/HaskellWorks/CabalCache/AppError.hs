{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.AppError
  ( AppError(..),
    NotFound(..),
    displayAppError,
    appErrorStatus,
  ) where

import Data.String                  (IsString(..))
import Data.Text                    (Text)
import GHC.Generics                 (Generic)
import HaskellWorks.CabalCache.Show (tshow)

import qualified Data.Text          as T
import qualified Network.HTTP.Types as HTTP

data NotFound = NotFound deriving (Eq, Show, Generic)

data AppError
  = AwsAppError
    { status :: HTTP.Status
    }
  | HttpAppError
    { status :: HTTP.Status
    }
  | RetriesFailedAppError
  | GenericAppError Text
  deriving (Eq, Show, Generic)

instance IsString AppError where
  fromString = GenericAppError . T.pack

displayAppError :: AppError -> Text
displayAppError (AwsAppError s)       = tshow s
displayAppError (HttpAppError s)      = tshow s
displayAppError RetriesFailedAppError = "Multiple retries failed"
displayAppError (GenericAppError msg) = msg

appErrorStatus :: AppError -> Maybe Int
appErrorStatus (AwsAppError (HTTP.Status statusCode _)) = Just statusCode
appErrorStatus _                                        = Nothing
