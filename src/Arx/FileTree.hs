module Arx.FileTree where

import Data.List

import System.Directory
import System.FilePath
import System.Posix
import Control.Monad (forM)

type FileName = String

data File i = File
  { fileName :: FileName
  , status   :: i
  } deriving Show

data Dir i = Dir
  { dir :: FileName
  , children :: [Node i]
  , status   :: i
  } deriving Show

data Node i = DirNode (Dir i) | FileNode (File i)
  deriving Show

instance Functor Node where
  fmap f (DirNode  Dir{..})  = DirNode $ Dir dir (fmap (fmap f) children) (f status)
  fmap f (FileNode File{..}) = FileNode $ File fileName (f status)


nodeName :: Node i -> FileName
nodeName (DirNode  Dir{..})  = dir
nodeName (FileNode File{..}) = fileName

nodeStatus :: Node a -> a
nodeStatus (DirNode  Dir{..})  = status
nodeStatus (FileNode File{..}) = status

-- | Modify the *FileNode* addressed by the given path.
-- Silently does nothing if path points to a directory, or if
-- it points to a non-existing thing.
modify :: (File i -> File i) -> [FileName] -> Node i -> Node i

modify f (p:[]) n@(FileNode file@File{..}) =
  if fileName == p 
    then FileNode (f file)
    else n
modify f (p:_:_) fn@(FileNode _) = fn

modify f []     n@(DirNode d@Dir{..}) = n
modify f (p:ps) n@(DirNode d@Dir{..}) =
  if p == dir
    then DirNode d{children = modify f ps <$> children}
    else n
    
modify' :: (File i -> File i) -> FilePath -> RootNode i -> RootNode i
modify' f p rn@RootNode{..} = 
  case stripPrefix (anchor <> [pathSeparator]) p of
    Just suffix -> rn { node = modify f (splitDirectories suffix) node }
    Nothing     -> rn

data RootNode i = RootNode
  { anchor   :: FilePath -- context
  , node     :: Node i
  } deriving Show

instance Functor RootNode where
  fmap f rn@RootNode{..} = rn { node = f <$> node }

type FilterPredicate = FilePath -> FileStatus -> Bool

isLeaf :: Node i -> Bool
isLeaf (DirNode Dir{..}) = null children

-- | Throws if file path does not exist.
-- The predicate is not applied to the given root path, only
-- to its descendants.
findTree :: (FileStatus -> i) -> FilePath -> FilterPredicate -> IO (Node i)
findTree info fp fi = do
  let name = takeBaseName fp

  st <- getFileStatus fp
  if isDirectory st
    then do
      fs  <- listDirectory fp
      chs <- forM fs (\bn -> do
        let child = fp </> bn
        st <- getFileStatus child

        -- only recurse on filtered children
        if fi child st
          then (: []) <$> findTree info child fi
          else return [])

      return (DirNode (Dir name (concat chs) (info st)))
    else do
      let name = takeFileName fp
      return (FileNode (File name (info st)))
