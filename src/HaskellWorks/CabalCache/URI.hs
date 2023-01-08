{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module HaskellWorks.CabalCache.URI
  ( reslashUri,
  ) where

import Control.Lens              ((&), (%~))
import Data.Generics.Product.Any (HasAny(the))
import Network.URI               (URI)

reslashUri :: URI -> URI
reslashUri uri = uri & the @"uriPath" %~ fmap reslashChar
  where reslashChar :: Char -> Char
        reslashChar '\\' = '/'
        reslashChar c    = c
