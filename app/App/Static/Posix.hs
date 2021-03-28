module App.Static.Posix where

import HaskellWorks.CabalCache.Location ((</>))

import qualified App.Static.Base as S

cabalDirectory :: FilePath
cabalDirectory = S.homeDirectory </> ".cabal"
