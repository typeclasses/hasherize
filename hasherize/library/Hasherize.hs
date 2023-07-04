module Hasherize (getHash) where

import Control.Monad (foldM)
import Control.Monad.Fail (fail)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (evalState, evalStateT, get, modify')
import Crypto.Hash.SHA256 qualified as Hash
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Function (fix, flip)
import Data.List (map)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Essentials
import System.Directory.OsPath qualified as Dir
import System.File.OsPath qualified as OsPath (withFile)
import System.IO qualified as IO
import System.OsPath (OsPath, (</>))
import System.OsPath qualified as OsPath
import Text.Show (show)

-- | Calculate the hash of a file or directory
getHash :: (MonadIO m) => OsPath -> m ByteString
getHash path = do
  files <- Map.toAscList <$> enumerateFiles path
  liftIO $ flip evalStateT Hash.init do
    for_ files \(k, v) -> do
      modify' $ flip Hash.update $ hashAbstractPath k
      modify' . flip Hash.update =<< getFileHash (path <//> v)
    Hash.finalize <$> get

getFileHash :: (MonadIO m) => OsPath -> m ByteString
getFileHash path = liftIO $ OsPath.withFile path IO.ReadMode \handle ->
  flip evalStateT Hash.init $ fix \continue -> do
    bs <- liftIO $ BS.hGetSome handle 4_096
    case BS.null bs of
      True -> Hash.finalize <$> get
      False -> do
        modify' $ flip Hash.update bs
        continue

newtype AbstractPath = AbstractPath (Seq Text)
  deriving newtype (Semigroup, Monoid, Eq, Ord)
  deriving stock (Show)

hashAbstractPath :: AbstractPath -> ByteString
hashAbstractPath (AbstractPath xs) = flip evalState Hash.init do
  for_ xs \x -> do
    modify' $ flip Hash.update $ Hash.hash $ Text.encodeUtf8 x
  Hash.finalize <$> get

abstractPathSingleton :: Text -> AbstractPath
abstractPathSingleton = AbstractPath . Seq.singleton

data OsPath' = This | OsPath' OsPath
  deriving stock (Show)

(<//>) :: OsPath -> OsPath' -> OsPath
(<//>) x = \case This -> x; OsPath' y -> (x </> y)

enumerateFiles :: forall m. (MonadIO m) => OsPath -> m (Map AbstractPath OsPath')
enumerateFiles x =
  getFType x >>= \case
    File -> pure (Map.singleton mempty This)
    Directory -> do
      let enumerateChild :: Map AbstractPath OsPath' -> OsPath -> m (Map AbstractPath OsPath')
          enumerateChild m name = do
            nameText <- Text.pack <$> liftIO (OsPath.decodeUtf name)

            let contextualize :: (AbstractPath, OsPath') -> (AbstractPath, OsPath')
                contextualize (k, v) = (abstractPathSingleton nameText <> k, OsPath' (name <//> v))

            m' <- enumerateFiles (x </> name)
            pure $ m <> Map.fromList (map contextualize (Map.toList m'))

      children <- liftIO $ Dir.listDirectory x

      foldM enumerateChild Map.empty children

data FType = File | Directory

getFType :: (MonadIO m) => OsPath -> m FType
getFType x =
  liftIO (Dir.doesFileExist x) >>= \case
    True -> pure File
    False ->
      liftIO (Dir.doesDirectoryExist x) >>= \case
        True -> pure Directory
        False -> liftIO (fail $ "Neither file nor directory: " <> show x)
