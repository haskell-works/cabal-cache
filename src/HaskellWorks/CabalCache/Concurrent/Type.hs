{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module HaskellWorks.CabalCache.Concurrent.Type
  ( DownloadQueue(..)
  , ConsumerId
  , ProviderId
  , PackageId
  ) where

import GHC.Generics
import HaskellWorks.CabalCache.Types (PackageId)

import qualified Control.Concurrent.STM                as STM
import qualified Data.Set                              as S
import qualified HaskellWorks.CabalCache.Data.Relation as R

type ConsumerId = PackageId
type ProviderId = PackageId

data DownloadQueue = DownloadQueue
  { tDependencies :: STM.TVar (R.Relation ConsumerId ProviderId)
  , tUploading    :: STM.TVar (S.Set PackageId)
  , tFailures     :: STM.TVar (S.Set PackageId)
  } deriving Generic
