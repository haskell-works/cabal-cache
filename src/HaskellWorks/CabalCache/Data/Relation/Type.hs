{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.CabalCache.Data.Relation.Type
  ( Relation (..)
  ) where

import GHC.Generics

import qualified Data.Map as M
import qualified Data.Set as S

data Relation a b = Relation
  { domain :: M.Map a (S.Set b)
  , range  :: M.Map b (S.Set a)
  } deriving (Eq, Show, Ord, Generic)
