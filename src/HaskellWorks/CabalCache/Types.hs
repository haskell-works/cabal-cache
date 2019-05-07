{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module HaskellWorks.CabalCache.Types where

import Data.Aeson
import Data.Maybe   (fromMaybe)
import Data.Text    (Text)
import GHC.Generics

type CompilerId = Text
type PackageId  = Text

data PlanJson = PlanJson
  { compilerId  :: CompilerId
  , installPlan :: [Package]
  } deriving (Eq, Show, Generic)

data Package = Package
  { packageType   :: Text
  , id            :: PackageId
  , name          :: Text
  , version       :: Text
  , style         :: Maybe Text
  , componentName :: Maybe Text
  , components    :: Maybe Components
  , depends       :: [Text]
  , exeDepends    :: [Text]
  } deriving (Eq, Show, Generic)

newtype Components = Components
  { lib :: Maybe Lib
  } deriving (Eq, Show, Generic)

data Lib = Lib
  { depends    :: [Text]
  , exeDepends :: [Text]
  } deriving (Eq, Show, Generic)

instance FromJSON PlanJson where
  parseJSON = withObject "PlanJson" $ \v -> PlanJson
    <$> v .: "compiler-id"
    <*> v .: "install-plan"

instance FromJSON Package where
  parseJSON = withObject "Package" $ \v -> do
    packageType   <- v .:  "type"
    id            <- v .:  "id"
    name          <- v .:  "pkg-name"
    version       <- v .:  "pkg-version"
    style         <- v .:? "style"
    componentName <- v .:? "component-name"
    components    <- v .:? "components"
    depends       <- v .:? "depends"     .!= []
    exeDepends    <- v .:? "exe-depends" .!= []
    return Package {..}

instance FromJSON Components where
  parseJSON = withObject "Components" $ \v -> Components
    <$> v .:? "lib"

instance FromJSON Lib where
  parseJSON = withObject "Lib" $ \v -> Lib
    <$> v .:? "depends"     .!= []
    <*> v .:? "exe-depends" .!= []
