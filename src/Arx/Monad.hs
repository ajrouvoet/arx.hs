{-# LANGUAGE OverloadedStrings #-}
module Arx.Monad where

-- import Data.UnixTime
import Data.Map.Strict as Map
import Data.Maybe
import Data.Default
import Data.Text (pack)
import Data.Time
import Data.Time.Clock.POSIX
-- import Data.Attoparsec.Char8 as Atto
-- import Data.Word
-- import Data.HashMap.Strict as HM
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BSC
-- import Data.ByteString.Base16 as Hex

import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

-- import System.IO
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
  config          :: m Config

type DbAction m a = ReaderT SqlBackend (NoLoggingT (ResourceT m)) a

latestSnap :: (MonadArx m) => DbAction m (Maybe (Entity Snap))
latestSnap = do
  selectFirst
    []
    [ Desc SnapFinished ]

mkSnap :: (MonadArx m) => m (Entity Snap)
mkSnap = do
  archive ← _root <$> config
  time ← liftIO $ getCurrentTime
  withDb $ insertEntity $ Snap archive time

withDb :: (MonadArx m) => DbAction m a → m a
withDb c = do
  path ← (\c → c^.dbPath) <$> config
  liftIO (createDirectoryIfMissing True $ takeDirectory path)
  runSqlite (pack path) c

init :: (MonadArx m) => m ()
init = do
  withDb $ runMigration migrateAll
  buildCache

getObject :: (MonadIO m) ⇒ FilePath → m PlainObject
getObject path = do
  dig ← liftIO $ hashFile path
  return $ Plain path dig

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
  withDb $ insertEntity $ Object snap path (show d)

copyFileCache :: (MonadArx m) => SnapId → SnapId → FilePath -> m (Entity Object)
copyFileCache snapFrom snapTo path = do
  do
    mObj ← withDb $ getBy (UniquePath snapFrom path)
    case mObj of
      Nothing  → do
        logWarnNS "arx:cache" (pack $ "Cache miss for " ++ path)
        buildFileCache snapTo path
      Just obj → withDb $ insertEntity $ Object snapTo path (objectDigest $ entityVal obj)

buildFreshCache :: (MonadArx m) => SnapId → [FilePath] → m ()
buildFreshCache snap files = forM_ files $ \ file → do
  logInfoNS "arx:cache" (pack $ "Fresh cache for " ++ file)
  buildFileCache snap file

buildCache :: (MonadArx m) => m ()
buildCache = do
  mLatest ← withDb latestSnap
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
  withDb $ do
    latest ← latestSnap
    case latest of
      Nothing   → return []
      Just snap → do
        let sid = entityKey snap
        selectList
          [ ObjectSnap   ==. sid
          , ObjectDigest ==. dig ]
          []

arx :: (MonadArx m) ⇒ Config → m a → LoggingT IO a
arx c m = do
  -- normalize the root
  r ← liftIO $ makeAbsolute (c^.root)
  runArx (Config r) m

-- missing :: Snapshot → Snapshot → Map (Digest SHA1) Object
-- missing mst cli = Map.difference  (byDigest cli) (byDigest mst)

-- instance Default Snapshot where
--   def = Snapshot Map.empty HM.empty

-- dupeOf :: [Object] → [[Object]]
-- dupeOf objs =
--   let
--     bysha = Map.fromListWith (++) ((\o → (digest o , [o])) <$> objs)
--     dupe  = Map.filter ((>1) . length) bysha
--   in Map.elems dupe

-- readSnapped :: (MonadIO m) ⇒ FilePath → m UnixTime
-- readSnapped sp = do
--   ex ← liftIO $ doesFileExist sp
--   if ex
--     then fromEpochTime . Posix.modificationTime <$> liftIO (getFileStatus sp)
--     else return $ UnixTime 0 0

-- objectParser :: Parser Object
-- objectParser = do
--   (pth:dig:xs) ← sepBy (Atto.takeWhile (notInClass ",\r\n")) (char ',')
--   let digest = fromJust (Hash.digestFromByteString (fst $ Hex.decode dig))
--   return (Object (BSC.unpack pth) digest)

-- objectsParser :: Parser [Object]
-- objectsParser = do
--   Atto.sepBy objectParser endOfLine

-- objectPrinter :: Object → BSC.ByteString
-- objectPrinter (Object pth dig) =
--   let digest = BSC.pack $ show dig
--   in (BSC.pack pth) <> BSC.singleton ',' <> digest

-- snapshotPrinter :: Snapshot → BSC.ByteString
-- snapshotPrinter sn =
--   foldMap (\o -> objectPrinter o <> BSC.singleton '\n') (HM.elems (byPath sn))

-- readSnapshot :: (MonadIO m) ⇒ FilePath → m Snapshot
-- readSnapshot pth = do
--   ex ← liftIO $ doesFileExist pth
--   if ex
--     then do
--       ls   ← BSC.lines <$> (liftIO $ BS.readFile pth)
--       objs ← forM ls $ \line →
--         case parseOnly (objectParser <* endOfInput) line of
--           Left  err → liftIO $ putStrLn err >> undefined -- TODO ?
--           Right obj → return obj
--       return $ snapFromList objs
--     else
--       return def

-- writeSnapshot :: (MonadIO m) ⇒ FilePath → Snapshot → m ()
-- writeSnapshot fp sn = do
--   liftIO $ BS.writeFile fp (snapshotPrinter sn)

-- getObject :: (MonadIO m) ⇒ FilePath → m Object
-- getObject fp = do
--   dig ← liftIO $ hashFile fp
--   return $ Object fp dig

-- snapFromList :: [Object] → Snapshot
-- snapFromList tps =
--   let
--     byPath   = HM.fromList $ fmap (\o → (path o , o)) tps
--     byDigest = Map.fromList $ fmap (\o → (digest o , o)) tps
--   in Snapshot byDigest byPath

-- numUnique :: Snapshot → Int
-- numUnique = Map.size . byDigest

-- archive :: Config → FilePath
-- archive c = c^.root

-- snapPath :: Config → FilePath
-- snapPath c = c^.root </> ".arx"

-- init :: (MonadArx m) ⇒ m

-- insertObject :: (MonadArx m) ⇒ Object → m ()
-- insertObject o = do
--   snapshotModify
--     (\(Snapshot l r) →
--        Snapshot
--          (Map.insert (digest o) o l)
--          (HM.insert (path o) o r))

-- updateSnapshot :: (MonadArx m) ⇒ m Snapshot
-- updateSnapshot = do
--   -- TODO drop deleted

--   fs   ← modifiedFiles
--   arch ← _root <$> config
--   logline $ show (length fs) ++ " files modified in archive " ++ show arch
--   forM_ fs $ \fp → do
--     -- TODO drop old entries

--     -- resummarize file
--     logline $ "Summarizing " ++ fp
--     obj ← getObject fp

--     -- update snapshot
--     insertObject obj

--   liftIO epochTime >>= (snappedAtModify . fromEpochTime)

--   snapshot

-- logline :: (MonadIO m) ⇒ String → m ()
-- logline = liftIO . hPutStrLn stderr

-- duplicates :: (MonadArx m) ⇒ m [[Object]]
-- duplicates = do
--   objs ← (HM.elems . byPath) <$> snapshot
--   return $ dupeOf objs
