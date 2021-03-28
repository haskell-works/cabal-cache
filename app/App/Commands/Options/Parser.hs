module App.Commands.Options.Parser where

import Antiope.Core               (FromText, fromText)
import App.Commands.Options.Types (VersionOptions (..))
import Options.Applicative

import qualified Data.Text as Text

optsVersion :: Parser VersionOptions
optsVersion = pure VersionOptions

text :: FromText a => ReadM a
text = eitherReader (fromText . Text.pack)
