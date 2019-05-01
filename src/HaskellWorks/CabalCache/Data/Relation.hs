module HaskellWorks.CabalCache.Data.Relation
  ( Relation(Relation)
  , empty
  , null
  , fromList
  , toList
  , singleton
  , insert
  , delete
  , domain
  , range
  , restrictDomain
  , restrictRange
  , withoutDomain
  , withoutRange
  ) where

import GHC.Generics
import HaskellWorks.CabalCache.Data.Relation.Type (Relation (Relation))
import Prelude                                    hiding (null)

import qualified Data.Map                                   as M
import qualified Data.Set                                   as S
import qualified HaskellWorks.CabalCache.Data.Relation.Type as R

empty :: Relation a b
empty = Relation M.empty M.empty

null :: Relation a b -> Bool
null = M.null . R.domain

fromList :: (Ord a, Ord b) => [(a, b)] -> Relation a b
fromList rs = Relation
  { R.domain  = M.fromListWith S.union $ map (\(x, y) -> (x, S.singleton y)) rs
  , R.range   = M.fromListWith S.union $ map (\(x, y) -> (y, S.singleton x)) rs
  }

toList :: Relation a b -> [(a, b)]
toList r = concatMap
  (\(x, y) -> zip (repeat x) (S.toList y))
  (M.toList (R.domain  r))

singleton :: a -> b -> Relation a b
singleton x y = Relation
  { R.domain  = M.singleton x (S.singleton y)
  , R.range   = M.singleton y (S.singleton x)
  }

insert :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
insert x y r = Relation
  { R.domain  = M.insertWith S.union x (S.singleton y) (R.domain r)
  , R.range   = M.insertWith S.union y (S.singleton x) (R.range  r)
  }

delete :: (Ord a, Ord b) =>  a -> b -> Relation a b -> Relation a b
delete x y r = r
  { R.domain  = M.update (justUnlessEmpty . S.delete y) x (R.domain r)
  , R.range   = M.update (justUnlessEmpty . S.delete x) y (R.range  r)
  }

domain ::  Relation a b -> S.Set a
domain r = M.keysSet (R.domain r)

range ::  Relation a b -> S.Set b
range r = M.keysSet (R.range r)

restrictDomain :: (Ord a, Ord b) => S.Set a -> Relation a b -> Relation a b
restrictDomain s r = R.Relation
  { R.domain = M.restrictKeys (R.domain r) s
  , R.range  = M.mapMaybe (justUnlessEmpty . S.intersection s) (R.range r)
  }

restrictRange :: (Ord a, Ord b) => S.Set b -> Relation a b -> Relation a b
restrictRange s r = R.Relation
  { R.domain  = M.mapMaybe (justUnlessEmpty . S.intersection s) (R.domain r)
  , R.range   = M.restrictKeys (R.range r) s
  }

withoutDomain :: (Ord a, Ord b) => S.Set a -> Relation a b -> Relation a b
withoutDomain s r = R.Relation
  { R.domain = M.withoutKeys (R.domain r) s
  , R.range  = M.mapMaybe (justUnlessEmpty . flip S.difference s) (R.range r)
  }

withoutRange :: (Ord a, Ord b) => S.Set b -> Relation a b -> Relation a b
withoutRange s r = R.Relation
  { R.domain  = M.mapMaybe (justUnlessEmpty . flip S.difference s) (R.domain r)
  , R.range   = M.withoutKeys (R.range r) s
  }

------

justUnlessEmpty :: S.Set a -> Maybe (S.Set a)
justUnlessEmpty c = if S.null c then Nothing else Just c
