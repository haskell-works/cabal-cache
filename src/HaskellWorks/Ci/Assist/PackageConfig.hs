{-# LANGUAGE OverloadedStrings #-}
module HaskellWorks.Ci.Assist.PackageConfig
where


import Data.ByteString.Char8       (pack)
import Data.ByteString.Lazy.Search (replace)
import HaskellWorks.Ci.Assist.Tar

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

storePathMacro :: BS.ByteString
storePathMacro = "${STORE_PATH}"

templateConfig :: FilePath -> LBS.ByteString -> LBS.ByteString
templateConfig storePath = replace (pack storePath) storePathMacro

unTemplateConfig :: FilePath -> LBS.ByteString -> LBS.ByteString
unTemplateConfig storePath = replace storePathMacro (pack storePath)
