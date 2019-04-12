{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module HaskellWorks.Ci.Assist.Core
  ( PackageInfo(..)
  , getPackages
  , relativePaths
  ) where

import Control.DeepSeq
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
import qualified HaskellWorks.Ci.Assist.Text  as T
import qualified HaskellWorks.Ci.Assist.Types as Z
import qualified System.Directory             as IO
import qualified System.IO                    as IO

type CompilerId = Text
type PackageId  = Text
type PackageDir = Text
type ConfPath   = Text
type Library    = Text

data PackageInfo = PackageInfo
  { compilerId :: CompilerId
  , packageId  :: PackageId
  , packageDir :: PackageDir
  , confPath   :: Maybe ConfPath
  , libs       :: [Library]
  } deriving (Show, Eq, Generic, NFData)

relativePaths :: PackageInfo -> [FilePath]
relativePaths pInfo = mempty
  <>  maybeToList (T.unpack <$> pInfo ^. the @"confPath")
  <>  [T.unpack (packageDir pInfo)]
  <>  (pInfo ^. the @"libs" <&> T.unpack)

getPackages :: Text -> Z.PlanJson -> IO [PackageInfo]
getPackages basePath planJson = forM packages (mkPackageInfo basePath compilerId)
  where compilerId :: Text
        compilerId = planJson ^. the @"compilerId"
        packages :: [Z.Package]
        packages = planJson ^.. the @"installPlan" . each . filtered predicate
        predicate :: Z.Package -> Bool
        predicate package = package ^. the @"packageType" /= "pre-existing" && package ^. the @"style" == Just "global"

mkPackageInfo :: Text -> CompilerId -> Z.Package -> IO PackageInfo
mkPackageInfo basePath cid pkg = do
  let pid               = pkg ^. the @"id"
  let compilerPath      = basePath <> "/" <> cid
  let relativeConfPath  = cid <> "/package.db/" <> pid <> ".conf"
  let absoluteConfPath  = basePath <> "/" <> relativeConfPath
  let libPath           = compilerPath <> "/lib"
  let relativeLibPath   = cid <> "/lib"
  let libPrefix         = "libHS" <> pid
  absoluteConfPathExists <- IO.doesFileExist (T.unpack absoluteConfPath)
  libPathExists <- IO.doesDirectoryExist (T.unpack libPath)
  libFiles <- getLibFiles relativeLibPath libPath libPrefix
  return PackageInfo
    { compilerId  = cid
    , packageId   = pid
    , packageDir  = cid <> "/" <> pid
    , confPath    = bool Nothing (Just relativeConfPath) absoluteConfPathExists
    , libs        = libFiles
    }

getLibFiles :: Text -> Text -> Text -> IO [Library]
getLibFiles relativeLibPath libPath libPrefix = fmap ((relativeLibPath <> "/") <>) . mfilter (T.isPrefixOf libPrefix) . fmap T.pack <$> IO.listDirectory (T.unpack libPath)
