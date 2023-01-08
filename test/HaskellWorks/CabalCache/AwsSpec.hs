{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module HaskellWorks.CabalCache.AwsSpec
  ( spec
  ) where

import Antiope.Core
import Antiope.Env
import Control.Lens
import Control.Monad
import Control.Monad.Except             (runExceptT)
import Control.Monad.IO.Class
import Data.Maybe                       (isJust)
import HaskellWorks.CabalCache.AppError
import HaskellWorks.CabalCache.IO.Lazy
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Control.Monad.Oops         as OO
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Variant               as OO
import qualified Hedgehog                   as H
import qualified Network.HTTP.Types         as HTTP
import qualified Network.URI                as URI
import qualified System.Environment         as IO

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.CabalCache.QuerySpec" do
  it "stub" $ requireTest do
    ci <- liftIO $ IO.lookupEnv "CI" <&> isJust
    unless ci do
      envAws <- liftIO $ mkEnv Oregon (const LBSC.putStrLn)
      let Just uri = URI.parseURI "s3://jky-mayhem/hjddhd"
      result :: Either (OO.Variant '[AppError, GenericError]) ()
        <- liftIO $ runExceptT $ OO.suspendM runResourceT $ void (headS3Uri envAws uri)

      case result of
        Right _ -> H.failure
        Left e -> OO.toEithers e === Left AwsAppError
          { status = HTTP.Status { HTTP.statusCode = 404 , HTTP.statusMessage = "Not Found" }
          }
