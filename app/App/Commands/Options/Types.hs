{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types
  ( CpOptions(..),
    PlanOptions(..),
    SyncFromArchiveOptions(..),
    SyncToArchiveOptions(..),
    VersionOptions(..),
  ) where

import Data.ByteString                  (ByteString)
import Data.Set                         (Set)
import GHC.Generics                     (Generic)
import HaskellWorks.CabalCache.Location (Location)
import Network.URI                      (URI)

import qualified Data.List.NonEmpty             as NEL
import qualified HaskellWorks.CabalCache.Types  as Z
import qualified Network.AWS                    as AWS

data CpOptions = CpOptions
  { region        :: AWS.Region
  , srcUri        :: URI
  , dstUri        :: URI
  , awsLogLevel   :: Maybe AWS.LogLevel
  , hostEndpoint  :: Maybe (ByteString, Int, Bool)
  } deriving (Eq, Show, Generic)

data SyncToArchiveOptions = SyncToArchiveOptions
  { region        :: AWS.Region
  , archiveUri    :: Location
  , path          :: FilePath
  , buildPath     :: FilePath
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , threads       :: Int
  , awsLogLevel   :: Maybe AWS.LogLevel
  , hostEndpoint  :: Maybe (ByteString, Int, Bool)
  , maxRetries    :: Int
  } deriving (Eq, Show, Generic)

data PlanOptions = PlanOptions
  { path          :: FilePath
  , buildPath     :: FilePath
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , outputFile    :: FilePath
  } deriving (Eq, Show, Generic)

data SyncFromArchiveOptions = SyncFromArchiveOptions
  { region          :: AWS.Region
  , archiveUris     :: NEL.NonEmpty Location
  , path            :: FilePath
  , buildPath       :: FilePath
  , storePath       :: FilePath
  , storePathHash   :: Maybe String
  , threads         :: Int
  , awsLogLevel     :: Maybe AWS.LogLevel
  , hostEndpoint    :: Maybe (ByteString, Int, Bool)
  , maxRetries      :: Int
  , ignorePackages  :: Set Z.PackageId
  } deriving (Eq, Show, Generic)

data VersionOptions = VersionOptions deriving (Eq, Show, Generic)
