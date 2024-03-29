module HaskellWorks.CabalCache.Options
  ( readOrFromTextOption,
  ) where

import Amazonka.Data.Text    (FromText (..), fromText)
import Control.Applicative   (Alternative(..))
import Options.Applicative   (Parser, Mod, OptionFields)
import Text.Read             (readEither)

import qualified Data.Text            as T
import qualified Options.Applicative  as OA

readOrFromTextOption :: (Read a, FromText a) => Mod OptionFields a -> Parser a
readOrFromTextOption =
  let fromStr s = readEither s <|> fromText (T.pack s)
  in OA.option $ OA.eitherReader fromStr
