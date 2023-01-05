module HaskellWorks.CabalCache.Data.List
  ( tuple2ToDL,
    tuple2ToList,
  ) where

tuple2ToDL :: (a, a) -> [a] -> [a]
tuple2ToDL (a, b) = (a:) . (b:)

tuple2ToList :: (a, a) -> [a]
tuple2ToList ab = tuple2ToDL ab []
