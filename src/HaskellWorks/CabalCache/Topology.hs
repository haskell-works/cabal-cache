{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.CabalCache.Topology
  ( PlanData(..)
  , buildPlanData
  , canShare
  ) where

import Control.Arrow                 ((&&&))
import Control.Lens                  (each, set, view, (&), (.~), (<&>), (^.), (^..))
import Control.Monad                 (join)
import Data.Either                   (fromRight)
import Data.Generics.Product.Any     (the)
import Data.Map.Strict               (Map)
import Data.Maybe                    (fromMaybe, mapMaybe)
import Data.Set                      (Set)
import Data.Text                     (Text)
import GHC.Generics                  (Generic)
import HaskellWorks.CabalCache.Types (Package, PackageId, PlanJson)

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import qualified Topograph       as TG

newtype PlanData = PlanData
  { nonShareable :: Set PackageId
  } deriving Generic

buildPlanData :: PlanJson   -- ^ The original plan
  -> [PackageId]            -- ^ Packages that are known to be non-shareable
  -> PlanData               -- ^ Updated plan
buildPlanData plan nonShareablePkgs =
  let dm = dependenciesMap (plan ^. the @"installPlan")
  in buildPlanData' dm nonShareablePkgs

canShare :: PlanData -> PackageId -> Bool
canShare planData pkgId = S.notMember pkgId (nonShareable planData)

-------------------------------------------------------------------------------

dependenciesMap :: [Package] -> Map PackageId (Set PackageId)
dependenciesMap plan = plan
  <&> (view (the @"id") &&& view (the @"depends"))
  <&> fmap S.fromList & M.fromList

buildPlanData' :: Map PackageId (Set PackageId) -- ^ Dependencies map
  -> [PackageId]                                -- ^ Packages to exclude
  -> PlanData                                   -- ^ All package ids to exclude
buildPlanData' plan knownNonShareable =
  fromRight (error "Could not process dependencies") $
    TG.runG plan $ \g ->
      let tg        = TG.transpose g
          nsPaths   = concatMap (fromMaybe [] . paths tg) knownNonShareable
          nsAll     = S.fromList (join nsPaths)
          dMap      = TG.adjacencyMap (TG.reduction g)
          rdMap     = TG.adjacencyMap (TG.reduction tg)
      in PlanData { nonShareable = nsAll }
  where paths g x = (fmap . fmap . fmap) (TG.gFromVertex g) $ TG.dfs g <$> TG.gToVertex g x
