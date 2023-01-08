module App.Commands.Options.Parser
  ( optsVersion,
    text,
  ) where

import App.Commands.Options.Types (VersionOptions (..))
import Options.Applicative        (Parser, ReadM)

import qualified Data.Text           as Text
import qualified Network.AWS.Data    as AWS
import qualified Options.Applicative as OA

optsVersion :: Parser VersionOptions
optsVersion = pure VersionOptions

text :: AWS.FromText a => ReadM a
text = OA.eitherReader (AWS.fromText . Text.pack)
