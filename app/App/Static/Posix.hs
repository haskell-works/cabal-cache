module App.Static.Posix
  ( cabalDirectory,
  ) where

import HaskellWorks.CabalCache.Location ((</>))
import HaskellWorks.Prelude

import qualified App.Static.Base as S

cabalDirectory :: FilePath
cabalDirectory = S.homeDirectory </> ".cabal"
