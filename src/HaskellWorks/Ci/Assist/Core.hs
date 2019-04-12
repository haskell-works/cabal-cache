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
import Control.Monad
import Data.Aeson
import Data.Bool
import Data.Generics.Product.Any
import Data.Maybe
import Data.Semigroup            ((<>))
import Data.Text                 (Text)
import GHC.Generics

import qualified Data.Text                    as T
import qualified HaskellWorks.Ci.Assist.Types as Z
import qualified System.Directory             as IO

type CompilerId = Text
type PackageId  = Text
type PackageDir = Text
type ConfPath   = Text

data PackageInfo = PackageInfo
  { compilerId :: CompilerId
  , packageId  :: PackageId
  , packageDir :: PackageDir
  , confPath   :: Maybe ConfPath
  } deriving (Show, Eq)

relativePaths :: PackageInfo -> [FilePath]
relativePaths pInfo =
  [ T.unpack (packageDir pInfo)
  ] <> maybeToList (T.unpack <$> confPath pInfo)

getPackages :: Text -> Z.PlanJson -> IO [PackageInfo]
getPackages basePath planJson = forM packageIds (mkPackageInfo basePath compilerId)
  where compilerId :: Text
        compilerId = planJson ^. the @"compilerId"
        packageIds :: [Text]
        packageIds = planJson ^.. the @"installPlan" . each . filtered predicate . the @"id"
        predicate :: Z.Package -> Bool
        predicate package = package ^. the @"packageType" /= "pre-existing" && package ^. the @"style" == Just "global"

mkPackageInfo :: Text -> CompilerId -> PackageId -> IO PackageInfo
mkPackageInfo basePath cid pid = do
  let relativeConfPath = cid <> "/package.db/" <> pid <> ".conf"
  let absoluteConfPath = basePath <> "/" <> relativeConfPath
  absoluteConfPathExists <- IO.doesFileExist (T.unpack absoluteConfPath)
  return PackageInfo
    { compilerId  = cid
    , packageId   = pid
    , packageDir  = cid <> "/" <> pid
    , confPath    = bool Nothing (Just relativeConfPath) absoluteConfPathExists
    }
