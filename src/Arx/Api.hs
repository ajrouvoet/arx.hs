{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Arx.Api where

import GHC.Generics

import Data.Aeson hiding (Object)
import qualified Data.Aeson as Ae
import Data.Yaml (encode, Parser)
import Data.Maybe
import Data.List (isPrefixOf)
import Data.Text.Lazy (pack, Text)
import Data.Default

import Control.Lens hiding ((.:), (.=))
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.Resource
import Control.Monad.Reader

import System.Directory
import System.Posix.Time
import System.Posix.Files as Posix
import System.FilePath
import System.FilePath.Find as Find
import System.ProgressBar

import qualified Crypto.Hash as Hash
import Crypto.Hash.Conduit (hashFile)

import Arx.FileTree as FT

import Debug.Trace

-- Arx archive settings
data Settings = Settings

instance Default Settings where
  def = Settings {}

makeLenses ''Settings

instance ToJSON Settings where
  toJSON s = object []

instance FromJSON Settings where
  parseJSON (Ae.Object v) = do
    return $ Settings {}

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
  
prettyErr :: NewErr -> Text
prettyErr (NotNew Obj{..}) = "Content already exists at " <> pack _path
prettyErr OutOfArchive     = "Cannot add a location outside of the archive."

class ( Monad m
      , MonadUnliftIO m
      , MonadIO m
      , MonadLogger m) ⇒ MonadArx m where

  -- Gets the settings
  root        :: m FilePath
  askSettings :: m Settings

  -- Check if some content is contained in the archive.
  -- The object always contains an absolute path.
  hasContent  :: Digest   -> m (Maybe Object)

  -- Add assumed new content to the archive.
  _linkObject :: Object   -> m ()

  -- Get a list of all objects stored
  -- All paths are absolute.
  stored      :: m [Object]

data Contained = Yes | No | Duplicate Object
  deriving (Show)

-- | Check if an entry is already in the archive.
-- If it is not, but
hasEntry :: (MonadArx m) => Object -> m Contained
hasEntry o = do
  o' <- hasContent (o ^. digest)
  case o' of
    Nothing        -> return No
    (Just another) -> return $ if another ^. path == o ^. path
      then Yes
      else Duplicate another

-- | Get the hash for a file.
-- Throws when path is of a directory
getObject :: (MonadIO m) ⇒ FilePath → m Object
getObject path = do
  dig :: Hash.Digest Hash.SHA1 ← liftIO $ hashFile path
  return $ Obj path (show dig)


newObject :: (MonadArx m) => Object -> m (Either NewErr Object)
newObject o = do
    -- check if o is in the archive
    r <- root
    let rooted = r `isPrefixOf` (o ^. path)

    if not rooted
      then return $ Left OutOfArchive
      else do
        -- check if it is new
        res <- hasContent (o ^. digest)
        case res of
          Just o' -> return $ Left (NotNew o')
          Nothing -> do
            -- add the content
            _linkObject o
            return $ Right o

new :: (MonadArx m) => FilePath -> m (Either NewErr Object)
new p = do
  obj <- getObject p
  newObject obj

-- | Create many new objects, aggregating errors in the result.
newObjects :: (MonadArx m) => [Object] → m [(Object,NewErr)]
newObjects fs = concat <$> forM fs (\f -> do
  result <- newObject f
  case result of
    Left err -> return [(f, err)]
    Right _  -> return [])

news :: (MonadArx m) => [FilePath] -> m [(Object, NewErr)]
news fs = do
  os <- forM fs getObject
  newObjects os

data Mark = Marked | NotMarked | Mixed
  deriving (Eq, Show)

joinMark :: Mark -> Mark -> Mark
joinMark NotMarked Marked  = Mixed
joinMark Marked NotMarked  = Mixed
joinMark Mixed _ = Mixed
joinMark _ Mixed = Mixed
joinMark Marked Marked = Marked
joinMark NotMarked NotMarked = NotMarked

markFiles :: [FilePath] -> RootNode Mark -> RootNode Mark
markFiles []     n = n
markFiles (f:fs) n =
  markFiles fs (modify' (\case File{..} -> File fileName Marked) f n)

-- | Delete stuff from the status that is too verbose
pruneMarks :: Node Mark -> Maybe (Node Mark)
pruneMarks (FileNode f)                       = Just $ FileNode f
pruneMarks (DirNode  Dir{..}) =
  let
    -- first prune the children
    chs  = concat (maybeToList . pruneMarks <$> children)
  in 
  case chs of 
    []   -> Nothing -- don't include empty dirs
    c:cs -> 
      -- join the marks for the children
      let m = foldl (\acc i -> joinMark acc (nodeStatus i)) (nodeStatus c) cs in
      Just $ DirNode (Dir dir chs m)

status :: (MonadArx m) => RootNode i -> m (Maybe (RootNode Mark))
status rn = do
  let rn' = fmap (const NotMarked) rn 
  st    <- fmap (^. path) <$> stored

  -- mark everything
  let rn@RootNode{..}  = markFiles st rn'
  return $ pruneMarks node <&> \n -> rn {node = n}


prettyNode :: FilePath -> Node Mark -> IO ()
prettyNode pf n | nodeStatus n == Marked    = return ()
                | nodeStatus n == NotMarked = putStrLn $ "\t" <> pf </> nodeDisplay n
                | nodeStatus n == Mixed     =
  case n of
    FileNode f -> putStrLn $ "\t" <> FT.fileName f -- ??
    DirNode  d -> forM_ (FT.children d) (prettyNode (pf </> dir d))

prettyStatus :: Maybe (RootNode Mark) -> IO ()
prettyStatus Nothing  = putStrLn "Everything archived!"
prettyStatus (Just RootNode{..}) = do
  putStrLn "Unarchived files:"
  prettyNode "" node