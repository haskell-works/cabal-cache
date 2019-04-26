module HaskellWorks.Ci.Assist.Metadata
where

import Control.Monad.IO.Class        (MonadIO, liftIO)
import HaskellWorks.Ci.Assist.Core   (PackageInfo (..))
import HaskellWorks.Ci.Assist.IO.Tar (TarGroup (..))
import System.FilePath               ((<.>), (</>))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified System.Directory     as IO

newtype Metadata = Metadata FilePath deriving (Show, Eq, Ord)

metaDir :: String
metaDir = "_METADATA"

createMetadata :: MonadIO m => FilePath -> PackageInfo -> m Metadata
createMetadata tempPath pkg = liftIO $ do
  let pkgMetaPath = tempPath </> T.unpack (compilerId pkg) </> metaDir </> packageDir pkg
  IO.createDirectoryIfMissing True (pkgMetaPath </> metaDir)
  pure (Metadata pkgMetaPath)

addMetadata :: MonadIO m => Metadata -> T.Text -> LBS.ByteString -> m ()
addMetadata (Metadata pkgMetaPath) key =
  liftIO . LBS.writeFile (pkgMetaPath </> metaDir </> T.unpack key)

getMetadataTarGroup :: MonadIO m => Metadata -> m TarGroup
getMetadataTarGroup (Metadata pkgMetaPath) = liftIO $ do
  metas <- IO.listDirectory pkgMetaPath
  pure (TarGroup pkgMetaPath metas)
