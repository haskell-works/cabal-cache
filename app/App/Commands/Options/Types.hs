{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Types
  ( CpOptions(..),
    PlanOptions(..),
    SyncFromArchiveOptions(..),
    SyncToArchiveOptions(..),
    VersionOptions(..),
  ) where

import Antiope.Env                      (Region)
import Data.ByteString                  (ByteString)
import GHC.Generics                     (Generic)
import HaskellWorks.CabalCache.Location (Location)
import Network.URI                      (URI)

import qualified Antiope.Env as AWS

data CpOptions = CpOptions
  { region        :: Region
  , srcUri        :: URI
  , dstUri        :: URI
  , awsLogLevel   :: Maybe AWS.LogLevel
  , hostEndpoint  :: Maybe (ByteString, Int, Bool)
  } deriving (Eq, Show, Generic)

data SyncToArchiveOptions = SyncToArchiveOptions
  { region        :: Region
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
  { region        :: Region
  , archiveUris   :: [Location]
  , path          :: FilePath
  , buildPath     :: FilePath
  , storePath     :: FilePath
  , storePathHash :: Maybe String
  , threads       :: Int
  , awsLogLevel   :: Maybe AWS.LogLevel
  , hostEndpoint  :: Maybe (ByteString, Int, Bool)
  , maxRetries    :: Int
  } deriving (Eq, Show, Generic)

data VersionOptions = VersionOptions deriving (Eq, Show, Generic)
