module App.Commands where

import App.Commands.SyncFromArchive
import App.Commands.SyncToArchive
import App.Commands.Version
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdSyncFromArchive
  <>  cmdSyncToArchive
  <>  cmdVersion
