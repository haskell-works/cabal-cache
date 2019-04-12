module HaskellWorks.Ci.Assist.Tar
where

import Codec.Archive.Tar
import Codec.Archive.Tar.Entry
import Data.Time.Clock.POSIX   (utcTimeToPOSIXSeconds)
import System.Directory        (Permissions (..), getModificationTime, getPermissions)

import qualified Data.ByteString.Lazy as LBS

packFileEntryWith :: (LBS.ByteString -> LBS.ByteString)   -- ^ Transform file content
  -> FilePath                                             -- ^ Full path to find the file on the local disk
  -> TarPath                                              -- ^ Path to use for the tar Entry in the archive
  -> IO Entry
packFileEntryWith transform filepath tarpath = do
  mtime   <- getModTime filepath
  perms   <- getPermissions filepath
  content <- LBS.readFile filepath
  return (fileEntry tarpath (transform content)) {
    entryPermissions = if executable perms then executableFilePermissions
                                           else ordinaryFilePermissions,
    entryTime = mtime
  }

getModTime :: FilePath -> IO EpochTime
getModTime path = (floor . utcTimeToPOSIXSeconds) <$> getModificationTime path

mapFileEntriesWith :: (FilePath -> Bool)
  -> (LBS.ByteString -> LBS.ByteString)
  -> Entries e
  -> Entries e
mapFileEntriesWith pred transform entries =
  flip mapEntriesNoFail entries $ \entry ->
    if pred (entryPath entry)
      then case entryContent entry of
          NormalFile bs size ->
            let bs' = transform bs
            in entry { entryContent = NormalFile bs' (LBS.length bs') }
          _ -> entry
      else entry
