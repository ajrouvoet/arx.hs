{-# LANGUAGE UndecidableInstances #-}
module Arx.Monad where

-- import Data.UnixTime
import Data.Maybe
import Data.Text (pack)
import Data.Time
import Data.Time.Clock.POSIX

import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

import System.Directory
import System.Posix.Time
import System.Posix.Files as Posix
import System.FilePath
import System.FilePath.Find as Find

import Database.Persist.Sqlite

import Crypto.Hash as Hash
import Crypto.Hash.Conduit

import Arx.Config
import Arx.Archive

-- | The Arx monad

class ( Monad m
      , MonadUnliftIO m
      , MonadIO m
      , MonadLogger m) ⇒ MonadArx m where

  runArx          :: Config → m a → LoggingT IO a
  runQuery        :: DbAction m a → m a
  config          :: m Config

type DbAction m a = ReaderT SqlBackend m a

latestSnap :: (MonadArx m) => DbAction m (Maybe (Entity Snap))
latestSnap = do
  selectFirst
    []
    [ Desc SnapFinished ]

mkSnap :: (MonadArx m) => m (Entity Snap)
mkSnap = do
  archive ← _root <$> config
  time ← liftIO $ getCurrentTime
  runQuery $ insertEntity $ Snap archive time

init :: (MonadArx m) => m ()
init = do
  path ← (\c → c^.dbPath) <$> config
  liftIO (createDirectoryIfMissing True $ takeDirectory path)
  runQuery $ runMigration migrateAll
  buildCache

getObject :: (MonadIO m) ⇒ FilePath → m PlainObject
getObject path = do
  dig :: Digest SHA1 ← liftIO $ hashFile path
  return $ Plain path (show dig)

utcToEpochTime = fromIntegral . floor . utcTimeToPOSIXSeconds

allFiles :: (MonadArx m) ⇒ m [FilePath]
allFiles = do
  path  ← _root <$> config
  files ← liftIO $ find always
    (   fileType ==? RegularFile
    &&? filePath /~? "**/.arx/*")
    path
  return ((\p → normalise (path </> p)) <$> files)

modifiedFiles :: (MonadArx m) ⇒ UTCTime → m [FilePath]
modifiedFiles since = do
  let time = utcToEpochTime since
  path  ← _root <$> config
  files ← liftIO $ find always
    (   fileType ==? RegularFile
    &&? filePath /~? "**/.arx/*"
    &&? Find.modificationTime >=? time)
    path
  return ((path </>) <$> files)

buildFileCache :: (MonadArx m) => SnapId → FilePath -> m (Entity Object)
buildFileCache snap path = do
  (Plain _ d) ← getObject path
  runQuery $ insertEntity $ Object snap path (show d)

copyFileCache :: (MonadArx m) => SnapId → SnapId → FilePath -> m (Entity Object)
copyFileCache snapFrom snapTo path = do
  do
    mObj ← runQuery $ getBy (UniquePath snapFrom path)
    case mObj of
      Nothing  → do
        logWarnNS "arx:cache" (pack $ "Cache miss for " ++ path)
        buildFileCache snapTo path
      Just obj → runQuery $ insertEntity $ Object snapTo path (objectDigest $ entityVal obj)

buildFreshCache :: (MonadArx m) => SnapId → [FilePath] → m ()
buildFreshCache snap files = forM_ files $ \ file → do
  logInfoNS "arx:cache" (pack $ "Fresh cache for " ++ file)
  buildFileCache snap file

buildCache :: (MonadArx m) => m ()
buildCache = do
  mLatest ← runQuery latestSnap
  snap    ← mkSnap

  -- We iterate over all files in the current archive.
  -- This ensures we will have cache entries if *and only if* the file currently
  -- exists. We will try to get a cache entry from an earlier snapshot if the
  -- modification time < latest snapshot time. If this fails, we create it
  -- freshly. Cache misses are reported.
  files   ← allFiles

  case mLatest of
    Nothing → buildFreshCache (entityKey snap) files
    Just latest → do
      let since = snapFinished $ entityVal latest

      forM_ files $ \ file → do
        status ← liftIO $ getFileStatus file
        let modded = Posix.modificationTime status > (utcToEpochTime since)

        if modded
          then (do
            logInfoNS "arx:cache" (pack $ "Changed: " ++ file)
            buildFileCache (entityKey snap) file
          ) else (do
            copyFileCache (entityKey latest) (entityKey snap) file
          )

checkFile :: (MonadArx m) ⇒ FilePath → m [Entity Object]
checkFile path = do
  obj ← getObject path
  checkDig (show $ digest obj)

checkDig :: (MonadArx m) ⇒ String → m [Entity Object]
checkDig dig = do
  runQuery $ do
    latest ← latestSnap
    case latest of
      Nothing   → return []
      Just snap → do
        let sid = entityKey snap
        selectList
          [ ObjectSnap   ==. sid
          , ObjectDigest ==. dig ]
          []

arx :: (MonadArx m) ⇒ Config → m a → IO a
arx c m = do
  -- normalize the root
  r ← liftIO $ makeAbsolute (c^.root)
  runStderrLoggingT $ runArx (Config r) m
