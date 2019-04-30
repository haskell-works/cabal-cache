{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.CabalCache.Version where

import Data.String

archiveVersion :: IsString s => s
archiveVersion = "v1"
