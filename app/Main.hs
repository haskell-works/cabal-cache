module Main where

import App.Commands         (commands)
import Control.Applicative  ((<**>))
import Control.Monad        (join)

import qualified Options.Applicative as OA

main :: IO ()
main = join $ OA.customExecParser
  (OA.prefs $ OA.showHelpOnEmpty <> OA.showHelpOnError)
  (OA.info (commands <**> OA.helper) OA.idm)
