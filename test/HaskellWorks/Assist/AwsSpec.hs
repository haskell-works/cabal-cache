{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Assist.AwsSpec
  ( spec
  ) where

import Antiope.Core
import Antiope.Env
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Generics.Product.Any
import Data.Maybe
import Data.Maybe                     (fromJust)
import HaskellWorks.Ci.Assist.IO.Lazy
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import System.Environment             (lookupEnv)
import Test.Hspec
import Text.RawString.QQ

import qualified Antiope.S3.Lazy              as LBS
import qualified Antiope.S3.Types             as AWS
import qualified Data.Aeson                   as A
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.ByteString.Lazy.Char8   as LBSC
import qualified HaskellWorks.Ci.Assist.Types as Z
import qualified System.Environment           as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Assist.QuerySpec" $ do
  it "stub" $ requireTest $ do
    ci <- liftIO $ IO.lookupEnv "CI" <&> isJust
    unless ci $ do
      envAws <- liftIO $ mkEnv Oregon (const LBSC.putStrLn)
      result <- liftIO $ runResourceT $ headS3Uri envAws $ AWS.S3Uri "jky-mayhem" "hjddhd"
      result === Left "Not found"
