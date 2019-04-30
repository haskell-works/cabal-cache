module HaskellWorks.CabalCache.Hash
  ( hashStorePath
  ) where

import qualified Crypto.Hash        as CH
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

hashStorePath :: String -> String
hashStorePath = take 10 . show . CH.hashWith CH.SHA256 . T.encodeUtf8 . T.pack
