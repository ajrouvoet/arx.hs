{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Arx.Api where

import GHC.Generics

import Data.Aeson hiding (Object)
import qualified Data.Aeson as Ae
import Data.Yaml (encode, Parser)
import Data.Maybe
import Data.Text (pack)
import qualified Data.ByteString as BS
import Data.Time
import Data.Time.Clock.POSIX
import Data.List.Split
import Data.Default

import Control.Lens hiding ((.:), (.=))
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
import System.ProgressBar

import qualified Crypto.Hash as Hash
import Crypto.Hash.Conduit (hashFile)

-- Arx archive settings
data Settings = Settings
  { _roots :: [FilePath]
  }

instance Default Settings where
  def = Settings
    { _roots = ["/"] -- masking nothing, can add any path on the system to the archive
    }

makeLenses ''Settings

instance ToJSON Settings where
  toJSON s = object [ "roots" .= (s ^. roots) ]

instance FromJSON Settings where
  parseJSON (Ae.Object v) = do
    roots <- (v .: "roots") :: Parser [FilePath]
    return $ Settings roots

-- The pointers of the archive
type Digest = String

-- An entry in the table
data Object = Obj
  { _path    :: String -- the value
  , _digest  :: Digest -- the key
  } deriving (Generic, Show)

makeLenses ''Object

instance ToJSON Object
instance FromJSON Object

-- | The Arx monad.
-- Insert-only archive with efficient de-duplication and contains checking.
-- It does not manage the files itself, it only knows about the locations.
-- This means that the client is responsible for ensuring that files do not get deleted or modified.

data NewErr = NotNew Object
            | OutOfArchive
  deriving (Show)

class ( Monad m
      , MonadUnliftIO m
      , MonadIO m
      , MonadLogger m) ⇒ MonadArx m where

  -- Gets the settings
  askSettings :: m Settings

  -- Check if some content is contained in the archive
  hasContent  :: Digest   -> m (Maybe Object)

  -- Add supposedly new content to the archive.
  newObject   :: Object   -> m (Either NewErr Object)

data Contained = Yes | No | Duplicate Object
  deriving (Show)

-- | Check if an entry is already in the archive.
-- If it is not, but
hasEntry :: (MonadArx m) => Object -> m Contained
hasEntry o = do
  o' <- hasContent (o ^. digest)
  case o' of
    Nothing        -> return No
    (Just another) -> return $ if (another ^. path) == (o ^. path)
      then Yes
      else Duplicate another

getObject :: (MonadIO m) ⇒ FilePath → m Object
getObject path = do
  dig :: Hash.Digest Hash.SHA1 ← liftIO $ hashFile path
  return $ Obj path (show dig)

new :: (MonadArx m) => FilePath -> m (Either NewErr Object)
new p = do
  obj <- getObject p
  newObject obj

-- | Create many new objects, aggregating errors in the result.
newObjects :: (MonadArx m) => [Object] → m [(Object,NewErr)]
newObjects fs = concat <$> (forM fs $ \f -> do
  result <- newObject f
  case result of
    Left err -> return [(f, err)]
    Right _  -> return [])

news :: (MonadArx m) => [FilePath] -> m [(Object, NewErr)]
news fs = do
  os <- forM fs getObject
  newObjects os

-- latestSnap :: (MonadArx m) => DbAction m (Maybe (Entity Snap))
-- latestSnap = do
--   selectFirst
--     []
--     [ Desc SnapFinished ]

-- mkSnap :: (MonadArx m) => m (Entity Snap)
-- mkSnap = do
--   archive ← _root <$> config
--   time ← liftIO $ getCurrentTime
--   runQuery $ insertEntity $ Snap archive time

-- init :: (MonadArx m) => m ()
-- init = do
--   path ← (\c → c^.dbPath) <$> config
--   liftIO (createDirectoryIfMissing True $ takeDirectory path)
--   runQuery $ runMigration migrateAll
--   overwriteSettings
--   buildCache

-- -- Given a string of format "<path>(\t<digest)?", we give a plain object.
-- -- If the digest is not provided, we recompute it.
-- parseOrGetObject :: (MonadIO m) ⇒ String → m PlainObject
-- parseOrGetObject line = do
--   case (splitOn "\t" line) of
--     -- Already have a hash
--     fp : dig : _ -> do
--       -- We validate the hash format
--       let dig' :: Digest SHA1 = read dig
--       return (Plain fp $ show dig')
--     fp : [] -> do
--       getObject fp

-- utcToEpochTime = fromIntegral . floor . utcTimeToPOSIXSeconds

-- allFiles :: (MonadArx m) ⇒ m [FilePath]
-- allFiles = do
--   path  ← _root <$> config
--   regularFilesOf path

-- modifiedFiles :: (MonadArx m) ⇒ UTCTime → m [FilePath]
-- modifiedFiles since = do
--   let time = utcToEpochTime since
--   path  ← _root <$> config
--   files ← liftIO $ find always
--     (   fileType ==? RegularFile
--     &&? filePath /~? "**/.arx/*"
--     &&? Find.modificationTime >=? time)
--     path
--   return ((path </>) <$> files)

-- buildFileCache :: (MonadArx m) => SnapId → FilePath -> m (Entity Object)
-- buildFileCache snap path = do
--   (Plain _ d) ← getObject path
--   runQuery $ insertEntity $ Object snap path (show d)

-- copyFileCache :: (MonadArx m) => SnapId → SnapId → FilePath -> m (Entity Object)
-- copyFileCache snapFrom snapTo path = do
--   do
--     mObj ← runQuery $ getBy (UniquePath snapFrom path)
--     case mObj of
--       Nothing  → do
--         logWarnNS "arx:cache" (pack $ "Cache miss for " ++ path)
--         buildFileCache snapTo path
--       Just obj → runQuery $ insertEntity $ Object snapTo path (objectDigest $ entityVal obj)

-- buildFreshCache :: (MonadArx m) => SnapId → [FilePath] → m ()
-- buildFreshCache snap files = forM_ files $ \ file → do
--   logInfoNS "arx:cache" (pack $ "Fresh cache for " ++ file)
--   buildFileCache snap file

-- overwriteSettings :: (MonadArx m) => m ()
-- overwriteSettings = do
--   c ← config
--   let p = c ^. getSettingsPath
--   liftIO $ BS.writeFile p (encode (_settings c))

-- buildCache :: (MonadArx m) => m ()
-- buildCache = do
--   mLatest ← runQuery latestSnap
--   snap    ← mkSnap

--   -- We iterate over all files in the current archive.
--   -- This ensures we will have cache entries if *and only if* the file currently
--   -- exists. We will try to get a cache entry from an earlier snapshot if the
--   -- modification time < latest snapshot time. If this fails, we create it
--   -- freshly. Cache misses are reported.
--   files   ← allFiles

--   -- report some progress
--   pb      ← liftIO $ newProgressBar defStyle 10 (Progress 0 (length files) ())

--   case mLatest of
--     Nothing → buildFreshCache (entityKey snap) files
--     Just latest → do
--       let since = snapFinished $ entityVal latest

--       forM_ files $ \ file → do
--         status ← liftIO $ getFileStatus file
--         let modded = Posix.modificationTime status > (utcToEpochTime since)

--         if modded
--           then (do
--             logInfoNS "arx:cache" (pack $ "Changed: " ++ file)
--             buildFileCache (entityKey snap) file
--           ) else (do
--             copyFileCache (entityKey latest) (entityKey snap) file
--           )

--         liftIO $ incProgress pb 1

-- checkFile :: (MonadArx m) ⇒ FilePath → m [Entity Object]
-- checkFile path = do
--   obj ← getObject path
--   checkDig (show $ digest obj)

-- checkDig :: (MonadArx m) ⇒ String → m [Entity Object]
-- checkDig dig = do
--   runQuery $ do
--     latest ← latestSnap
--     case latest of
--       Nothing   → return []
--       Just snap → do
--         let sid = entityKey snap
--         selectList
--           [ ObjectSnap   ==. sid
--           , ObjectDigest ==. dig ]
--           []
