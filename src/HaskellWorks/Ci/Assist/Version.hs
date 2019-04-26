{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Ci.Assist.Version where

import Data.String

archiveVersion :: IsString s => s
archiveVersion = "v1"
