{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Ci.Assist.Core
  ( PackageInfo(..)
  , getPackages
  , relativePaths
  ) where

import Control.Lens
import Data.Aeson
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Data.Text                 (Text)
import GHC.Generics

import qualified Data.Text                    as Text
import qualified HaskellWorks.Ci.Assist.Types as Z

data PackageInfo = PackageInfo
  { compilerId :: !Text
  , packageId  :: !Text
  , packageDir :: !Text
  , confPath   :: !Text
  } deriving (Show, Eq)

relativePaths :: PackageInfo -> [FilePath]
relativePaths pInfo =
  [ Text.unpack (packageDir pInfo)
  , Text.unpack (confPath pInfo)
  ]

getPackages :: Z.PlanJson -> [PackageInfo]
getPackages planJson = mkPackageInfo compilerId <$> packageIds
  where
    compilerId :: Text
    compilerId = planJson ^. the @"compilerId"
    packageIds :: [Text]
    packageIds = planJson ^.. the @"installPlan" . each . filtered predicate . the @"id"
    predicate :: Z.Package -> Bool
    predicate package = package ^. the @"packageType" /= "pre-existing" && package ^. the @"style" == Just "global"

    mkPackageInfo cid pid = PackageInfo
      { compilerId  = cid
      , packageId   = pid
      , packageDir  = cid <> "/" <> pid
      , confPath    = cid <> "/package.db/" <> pid <> ".conf"
      }
