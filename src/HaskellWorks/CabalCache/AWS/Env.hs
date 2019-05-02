module HaskellWorks.CabalCache.AWS.Env
  ( awsLogger
  ) where

import Antiope.Env (LogLevel (..))

import qualified Data.ByteString.Lazy               as LBS
import qualified Data.ByteString.Lazy.Char8         as LC8
import qualified Data.Text.Encoding                 as T
import qualified HaskellWorks.CabalCache.IO.Console as CIO
import qualified System.IO                          as IO

awsLogger :: LogLevel -> LC8.ByteString -> IO ()
awsLogger _ message = CIO.hPutStrLn IO.stderr $ T.decodeUtf8 $ LBS.toStrict message
