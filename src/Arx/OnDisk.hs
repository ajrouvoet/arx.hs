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
import System.Directory (createDirectory, doesFileExist, listDirectory, doesPathExist, getCurrentDirectory, canonicalizePath, createDirectoryIfMissing)

import Arx.Api

import Data.Aeson.Types as Aeson hiding (Object)
import System.Posix.Files
import System.Exit (exitFailure)

import Debug.Trace

arxDir = ".arx"

data Config = Config
  { _storePath :: FilePath
  , _settings  :: Settings
  }

makeLenses ''Config

logPath :: Getter Config FilePath
logPath = storePath . to (</> ".log")

withSettings :: FilePath -> FilePath
withSettings arch = arch </> ".settings.yaml"

getSettingsPath :: Getter Config FilePath
getSettingsPath = storePath . to withSettings

type OnDisk = ReaderT Config (LoggingT IO)

store :: OnDisk FilePath
store = do
  c <- ask
  return (c ^. storePath)

create :: Config -> IO Bool
create conf = do
  let s = conf ^. storePath
  exist  <- doesFileExist s
  if exist
    then return False
    else do
      createDirectoryIfMissing False s
      encodeFile (conf ^. getSettingsPath) (conf ^. settings)
      return True

runArxOnDisk :: FilePath -> OnDisk a -> IO a
runArxOnDisk root m = do
  let arch = root </> arxDir
  s <- decodeFileThrow (withSettings arch)
  let c = Config arch s

  runFileLoggingT (c ^. logPath) (runReaderT m c)

findArxRoot :: IO FilePath
findArxRoot = do
  path ← getCurrentDirectory
  findRoot path
  where
    findRoot :: FilePath → IO FilePath
    findRoot p = do
      let root = p </> arxDir
      exists ← doesPathExist root
      if exists
        then do
          return p
        else
          if p == "/"
          then do putStrLn "Not in an Arxive"; exitFailure
          else findRoot (takeDirectory p)

arx :: OnDisk a -> IO a
arx ma = do
  r  <- findArxRoot
  runArxOnDisk r ma

-- Implementation of MonadArx that builds the content-addressed mapping
-- on the disk.
instance MonadArx OnDisk where

  stored = do
    st  <- store
    ls  <- liftIO $ listDirectory st
    os  <- forM ls $ \l -> do
      let linkPath = st </> l
      fs <- liftIO $ getSymbolicLinkStatus linkPath

      -- hidden files are not part of the archive
      -- non-symbolic links are also ignored
      
      if take 1 (takeFileName l) /= "." && isSymbolicLink fs
        then do
          path <- liftIO $ readSymbolicLink linkPath
          path <- liftIO $ canonicalizePath (takeDirectory linkPath </> path)
          return [ Obj path (takeFileName l) ]
        else return []

    return $ concat os

  askSettings = do
    c <- ask
    return (c ^. settings)

  hasContent d = do
    s <- store
    let addr = s </> d
    hit <- liftIO $ fileExist (traceShowId addr)
    if (traceShowId hit)
      then do
        path <- liftIO $ readSymbolicLink addr
        path <- liftIO $ canonicalizePath (s </> path)
        return $ Just $ Obj path d
      else do
        return Nothing

  _linkObject o = do
    r <- root
    s <- store
    let src = ".." </> makeRelative r (o ^. path) -- TODO correct?
    let tgt = s </> (o ^. digest)
    liftIO $ createSymbolicLink src tgt