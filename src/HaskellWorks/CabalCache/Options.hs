module HaskellWorks.CabalCache.Options
  ( readOrFromTextOption
  ) where

import Network.AWS.Data.Text (FromText (..), fromText)
import Options.Applicative   hiding (columns)
import Text.Read             (readEither)

import qualified Data.Text as T

readOrFromTextOption :: (Read a, FromText a) => Mod OptionFields a -> Parser a
readOrFromTextOption =
  let fromStr s = readEither s <|> fromText (T.pack s)
  in option $ eitherReader fromStr
