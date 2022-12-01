module App.Static
  ( cabalDirectory
  , buildPath
  , path
  ) where

import qualified App.Static.Base    as S
import qualified App.Static.Posix   as P
import qualified App.Static.Windows as W

cabalDirectory :: FilePath
cabalDirectory = if S.isPosix then P.cabalDirectory else W.cabalDirectory

buildPath :: FilePath
buildPath = "dist-newstyle"

path :: FilePath
path = "."
