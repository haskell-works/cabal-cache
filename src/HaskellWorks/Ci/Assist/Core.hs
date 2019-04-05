{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.Ci.Assist.Core
  ( toPackageDirectories
  ) where

import Control.Lens
import Data.Aeson
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Data.Text                 (Text)
import GHC.Generics

import qualified HaskellWorks.Ci.Assist.Types as Z

toPackageDirectories :: Z.PlanJson -> [Text]
toPackageDirectories planJson = mkPackageDirectory <$> packageIds
  where compilerId :: Text
        compilerId = planJson ^. the @"compilerId"
        packageIds :: [Text]
        packageIds = planJson ^.. the @"installPlan" . each . filtered predicate . the @"id"
        mkPackageDirectory :: Text -> Text
        mkPackageDirectory packageId = compilerId <> "/" <> packageId
        predicate :: Z.Package -> Bool
        predicate package = package ^. the @"packageType" /= "pre-existing" && package ^. the @"style" == Just "global"
