{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Version
  ( cmdVersion
  ) where

import App.Commands.Options.Parser (optsVersion)
import App.Static                  (homeDirectory)
import Control.Lens                hiding ((<.>))
import Control.Monad               (unless, when)
import Data.Generics.Product.Any   (the)
import Data.List
import Data.Semigroup              ((<>))
import Options.Applicative         hiding (columns)
import Paths_hw_ci_assist

import qualified App.Commands.Options.Types        as Z
import qualified Data.Text                         as T
import qualified Data.Version                      as V
import qualified HaskellWorks.Ci.Assist.IO.Console as CIO

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runVersion :: Z.VersionOptions -> IO ()
runVersion _ = do
  let V.Version {..} = Paths_hw_ci_assist.version

  let version = intercalate "." $ fmap show versionBranch

  CIO.putStrLn $ "hw-ci-assist " <> T.pack version

cmdVersion :: Mod CommandFields (IO ())
cmdVersion = command "version"  $ flip info idm $ runVersion <$> optsVersion
