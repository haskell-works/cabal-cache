{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module HaskellWorks.CabalCache.Data.RelationSpec
  ( spec
  ) where

import HaskellWorks.CabalCache.Data.Relation (Relation (Relation))
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.List                             as L
import qualified Data.Map                              as M
import qualified Data.Set                              as S
import qualified HaskellWorks.CabalCache.Data.Relation as R
import qualified Hedgehog.Gen                          as G
import qualified Hedgehog.Range                        as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Assist.Data.RelationSpec" $ do
  it "List roundtrip" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha
    L.sort (R.toList (R.fromList as)) === L.sort as
  it "Full domain restriction" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha

    R.restrictDomain S.empty (R.fromList as) === R.empty
  it "Full range restriction" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha

    R.restrictRange S.empty (R.fromList as) === R.empty
  it "No domain restriction" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha
    let r = R.fromList as

    R.restrictDomain (R.domain r) r === r
  it "No range restriction" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha
    let r = R.fromList as
    R.restrictRange (R.range r) r === r
  it "Full domain without" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha
    let r = R.fromList as
    R.withoutDomain S.empty r === r
  it "Full range without" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha
    let r = R.fromList as
    R.withoutRange S.empty r === r
  it "No domain without" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha
    let r = R.fromList as

    R.withoutDomain (R.domain r) r === R.empty
  it "No range without" $ require $ property $ do
    as <- forAll $ G.list (R.linear 0 10) $ (,)
      <$> G.int R.constantBounded
      <*> G.alpha
    let r = R.fromList as
    R.withoutRange (R.range r) r === R.empty
