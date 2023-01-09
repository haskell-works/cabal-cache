{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.CabalCache.AppError
  ( AwsError(..),
    HttpError(..),
    HasStatusCode(..),
    displayAwsError,
    displayHttpError,
  ) where

import Data.Text                    (Text)
import GHC.Generics                 (Generic)
import HaskellWorks.CabalCache.Show (tshow)

import qualified Network.HTTP.Types as HTTP

newtype AwsError = AwsError
  { status :: HTTP.Status
  }
  deriving (Eq, Show, Generic)

newtype HttpError = HttpError
  { status :: HTTP.Status
  }
  deriving (Eq, Show, Generic)

displayAwsError :: AwsError -> Text
displayAwsError (AwsError s) = tshow s

displayHttpError :: HttpError -> Text
displayHttpError (HttpError s) = tshow s

class HasStatusCode a where
  statusCodeOf :: a -> Int

instance HasStatusCode AwsError where
  statusCodeOf (AwsError (HTTP.Status c _)) = c

instance HasStatusCode HttpError where
  statusCodeOf (HttpError (HTTP.Status c _)) = c
