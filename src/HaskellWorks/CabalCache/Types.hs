{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.CabalCache.Types where

import Data.Aeson
import Data.Text    (Text)
import GHC.Generics

data PlanJson = PlanJson
  { compilerId  :: Text
  , installPlan :: [Package]
  } deriving (Eq, Show, Generic)

data Package = Package
  { packageType   :: Text
  , id            :: Text
  , name          :: Text
  , version       :: Text
  , style         :: Maybe Text
  , componentName :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON PlanJson where
  parseJSON = withObject "PlanJson" $ \v -> PlanJson
    <$> v .: "compiler-id"
    <*> v .: "install-plan"

instance FromJSON Package where
  parseJSON = withObject "Package" $ \v -> Package
    <$> v .:  "type"
    <*> v .:  "id"
    <*> v .:  "pkg-name"
    <*> v .:  "pkg-version"
    <*> v .:? "style"
    <*> v .:? "component-name"
