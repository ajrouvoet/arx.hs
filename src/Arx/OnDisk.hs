module Arx.OnDisk where

import Data.List
import Data.Default
import Data.UnixTime
import Data.Yaml (encodeFile, decodeFileThrow)

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger

import System.Posix.Files as Posix
import System.Posix.Types
import System.FilePath
import System.Directory (createDirectory, doesFileExist)

import Arx.Api

import Data.Aeson.Types as Aeson hiding (Object)
import System.Posix.Files

data Config = Config
  { _storePath :: FilePath
  , _settings  :: Settings
  }

makeLenses ''Config

logPath :: Getter Config FilePath
logPath = storePath . to (\arch -> arch </> ".log")

withSettings :: FilePath -> FilePath
withSettings arch = arch </> ".settings.yaml"

getSettingsPath :: Getter Config FilePath
getSettingsPath = storePath . to withSettings

type OnDisk = ReaderT Config (LoggingT IO)

store :: OnDisk FilePath
store = do
  c <- ask
  return (c ^. storePath)

linkObject :: Object -> OnDisk  ()
linkObject o = do
  s <- store
  let tgt = s </> (o ^. digest)
  liftIO $ createSymbolicLink (o ^. path) tgt

create :: Config -> IO Bool
create conf = do
  let s = conf ^. storePath
  exist  <- doesFileExist s
  if exist
    then return False
    else do
      createDirectory s
      encodeFile (conf ^. getSettingsPath) (conf ^. settings)
      return True

runArxOnDisk :: FilePath -> OnDisk a -> IO a
runArxOnDisk archive m = do
  s <- decodeFileThrow (withSettings archive)
  let c = Config archive s

  runFileLoggingT (c ^. logPath) (runReaderT m c)

-- Implementation of MonadArx that builds the content-addressed mapping
-- on the disk.
instance MonadArx OnDisk where

  askSettings = do
    c <- ask
    return (c ^. settings)

  hasContent d = do
    s <- store
    let addr = s </> d
    hit <- liftIO $ fileExist addr
    if hit
      then do
        path <- liftIO $ readSymbolicLink addr
        return $ Just $ Obj path d
      else do
        return Nothing

  newObject o = do
    -- check if o is in the archive
    rs <- view (settings . roots)
    let rooted = any (\root -> root `isPrefixOf` (o ^. path)) rs

    if not rooted
      then return $ Left OutOfArchive
      else do
        -- check if it is new
        res <- hasContent (o ^. digest)
        case res of
          Just o' -> return $ Left (NotNew o')
          Nothing -> do
            -- add the content
            linkObject o
            return $ Right o
