module HaskellWorks.Ci.Assist.IO.Console
  ( putStrLn
  , print
  , hPutStrLn
  , hPrint
  ) where

import Control.Exception (bracket_)
import Data.Text         (Text)
import Prelude           (IO, Show (..), ($))

import qualified Control.Concurrent.QSem as IO
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified System.IO               as IO
import qualified System.IO.Unsafe        as IO

sem :: IO.QSem
sem = IO.unsafePerformIO $ IO.newQSem 1
{-# NOINLINE sem #-}

consoleBracket :: IO a -> IO a
consoleBracket = bracket_ (IO.waitQSem sem) (IO.signalQSem sem)

putStrLn :: Text -> IO ()
putStrLn text = consoleBracket $ T.putStrLn text

print :: Show a => a -> IO ()
print a = consoleBracket $ IO.print a

hPutStrLn :: IO.Handle -> Text -> IO ()
hPutStrLn h text = consoleBracket $ T.hPutStrLn h text

hPrint :: Show a => IO.Handle -> a -> IO ()
hPrint h a = consoleBracket $ IO.hPrint h a
