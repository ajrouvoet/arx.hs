{-# LANGUAGE OverloadedStrings #-}
module Arx.Monad where

-- import Data.UnixTime
import Data.Map.Strict as Map
import Data.Maybe
import Data.Default
import Data.Text
-- import Data.Attoparsec.Char8 as Atto
-- import Data.Word
-- import Data.HashMap.Strict as HM
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Char8 as BSC
-- import Data.ByteString.Base16 as Hex

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift

-- import System.IO
import System.Directory
-- import System.Posix.Files as Posix
-- import System.Posix.Types
-- import System.Posix.Time
import System.FilePath
-- import System.FilePath.Find as Find

import Database.Persist.Sqlite

import Crypto.Hash as Hash
import Crypto.Hash.Conduit

import Arx.Config
import Arx.Archive

-- | The Arx monad

class (Monad m, MonadUnliftIO m, MonadIO m) ⇒ MonadArx m where

  runArx          :: Config → m a → IO a

  -- configuration
  config          :: m Config
  -- snappedAt       :: m UnixTime
  -- snappedAtModify :: UnixTime → m ()

  -- snapshot       :: m Snapshot
  -- snapshotModify :: (Snapshot → Snapshot) → m ()

init :: (MonadArx m) => m ()
init = do
  path ← (\c → c^.dbPath) <$> config
  liftIO (createDirectoryIfMissing True $ takeDirectory path)
  runSqlite (pack path) $ do
    runMigration migrateAll

-- data Snapshot = Snapshot
--   { byDigest :: Map (Digest SHA1) Object
--   , byPath   :: HashMap FilePath Object
--   }

-- instance Show Snapshot where
--   show = show . byPath

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

-- modifiedFiles :: (MonadArx m) ⇒ m [FilePath]
-- modifiedFiles = do
--   time ← toEpochTime <$> snappedAt
--   root ← archive <$> config
--   liftIO $ find always
--     (   fileType ==? RegularFile
--     &&? fileName /=? ".arx"
--     &&? Find.modificationTime >=? time)
--     root

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

arx :: (MonadArx m) ⇒ Config → m a → IO a
arx c m = do
  runArx c m
