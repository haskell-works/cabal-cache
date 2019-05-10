{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.AppError
  ( AppError(..)
  , displayAppError
  , appErrorStatus
  ) where

import Data.String
import Data.Text                    (Text)
import GHC.Generics
import HaskellWorks.CabalCache.Show

import qualified Data.Text          as T
import qualified Network.HTTP.Types as HTTP

data AppError
  = AwsAppError
    { status :: HTTP.Status
    }
  | RetriesFailedAppError
  | GenericAppError Text
  deriving (Eq, Show, Generic)

instance IsString AppError where
  fromString = GenericAppError . T.pack

displayAppError :: AppError -> Text
displayAppError (AwsAppError status)  = tshow status
displayAppError RetriesFailedAppError = "Multiple retries failed"
displayAppError (GenericAppError msg) = msg

appErrorStatus :: AppError -> Maybe Int
appErrorStatus (AwsAppError (HTTP.Status statusCode _)) = Just statusCode
appErrorStatus _                                        = Nothing
