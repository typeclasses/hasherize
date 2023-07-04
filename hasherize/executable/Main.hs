module Main (main) where

import Control.Concurrent (getNumCapabilities)
import Control.Exception.Safe (bracket_, displayException)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.STM (atomically)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as LBS
import Data.Csv qualified as CSV
import Data.Csv.Builder qualified as CSV.Builder
import Data.Either (Either (..))
import Data.Foldable (for_)
import Data.String (String)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as Vector
import Env qualified
import Essentials
import Hasherize (getHash)
import Ki qualified
import QSem (newQSem, signalQSem, waitQSem)
import System.Directory.OsPath qualified as Dir
import System.File.OsPath qualified as OsPath (withFile)
import System.IO (Handle, IO)
import System.IO qualified as IO
import System.OsPath (OsPath, (</>))
import System.OsPath qualified as OsPath
import System.OsString (OsString, osstr)
import Unfork (unforkAsyncIO_)
import Prelude ((*))

main :: IO ()
main = do
  -- Read environment variables
  Env {inputDirectory, outputDirectory, outputManifest} <- getHasherizeArgs

  -- Get a list of all the inputs
  inputNameList <- Dir.listDirectory inputDirectory

  capabilities <- getNumCapabilities

  -- How many threads
  let concurrency = capabilities * 2

  -- Set up a thread-safe function to write to the manifest CSV
  withManifestWriter outputManifest \writeToManifest' ->
    -- Establish a thread scope to start forking threads
    liftIO $ Ki.scoped \scope -> do
      semaphore <- newQSem concurrency

      -- Loop over the list of inputs
      for_ inputNameList \(inputName :: OsString) ->
        bracket_ (waitQSem semaphore) (signalQSem semaphore) $
          -- Start one thread per input
          Ki.fork scope do
            -- Create a symlink from original path to hashed path
            entry <- hasherize inputDirectory outputDirectory inputName
            -- Record the mapping in the manifest CSV
            writeToManifest' entry

      -- Wait for all threads to finish
      atomically $ Ki.awaitAll scope

hasherize ::
  (MonadIO m) =>
  -- | Input directory
  OsPath ->
  -- | Output directory
  OsPath ->
  -- | Name of file or directory being hasherized
  OsPath ->
  m (ManifestEntry ByteString)
hasherize inDirectory outDirectory inputName = do
  inputNameString :: String <- liftIO $ OsPath.decodeUtf inputName
  let inputNameText :: Text = Text.pack inputNameString

  -- How we'll refer to the input in filesystem operations
  let inputPath = inDirectory </> inputName

  -- Hash of the content
  hashText :: Text <- do
    hash :: ByteString <- getHash inputPath
    pure $ Text.decodeUtf8 $ Base16.encode hash

  -- The new input name with the hash included
  nameWithHash :: OsPath <- do
    hashOsString :: OsString <- liftIO $ OsPath.encodeUtf $ Text.unpack hashText
    pure $ hashOsString <> [osstr|-|] <> inputName

  -- Place the content in the output directory under its new alias
  createLink inputPath (outDirectory </> nameWithHash)

  -- Return the mapping to store in the manifest CSV
  pure $
    fmap Text.encodeUtf8 $
      ManifestEntry inputNameText (hashText <> "-" <> inputNameText)

data ManifestEntry a = ManifestEntry a a
  deriving stock (Functor, Foldable, Traversable)

instance CSV.ToRecord (ManifestEntry ByteString) where
  toRecord (ManifestEntry k v) = Vector.fromList [k, v]

createLink :: (MonadIO m) => OsPath -> OsPath -> m ()
createLink target source = do
  isDir <- liftIO $ Dir.doesDirectoryExist target
  target' <- liftIO $ Dir.makeAbsolute target
  liftIO $ (if isDir then Dir.createDirectoryLink else Dir.createFileLink) target' source

withManifestWriter ::
  (MonadIO m) =>
  OsPath ->
  ((ManifestEntry ByteString -> IO ()) -> IO a) ->
  m a
withManifestWriter path continue = liftIO $
  OsPath.withFile path IO.WriteMode \handle ->
    liftIO $ unforkAsyncIO_ (writeToManifest handle) continue

writeToManifest :: Handle -> ManifestEntry ByteString -> IO ()
writeToManifest handle =
  LBS.hPutStr handle . BSB.toLazyByteString . CSV.Builder.encodeRecord

data Env = Env
  { inputDirectory :: OsString,
    outputDirectory :: OsString,
    outputManifest :: OsString
  }

getHasherizeArgs :: (MonadIO m) => m Env
getHasherizeArgs = liftIO $ Env.parse id do
  inputDirectory <- Env.var pathReader "input-directory" mempty
  outputDirectory <- Env.var pathReader "output-directory" mempty
  outputManifest <- Env.var pathReader "output-manifest" mempty
  pure Env {inputDirectory, outputDirectory, outputManifest}

pathReader :: Env.Reader Env.Error OsPath
pathReader string = case OsPath.encodeUtf string of
  Left e -> Left $ Env.unread $ displayException e
  Right x -> Right x
