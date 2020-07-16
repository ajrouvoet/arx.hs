module Main where

import Control.Monad
import Control.Monad.IO.Class

import Data.Char (toLower)
import Data.Hashable
import Data.Map.Strict as Map
import Data.ByteArray ()

import System.IO
import System.FilePath
import System.FilePath.Find as Find
import System.Directory
import System.Posix.Files as Posix
import Options.Applicative

import Debug.Trace

import Arx
import qualified Arx as Arx

data Command
  = Init Config
  -- Dupe Config
  -- | Snap Config
  -- | Diff BinaryOpts
  -- | Init 

data Filter = Filter { extensions :: String }
data BinaryOpts = Binary { left :: String , right :: String , inc :: Bool }

fileFilters :: Parser Filter
fileFilters = Filter <$> argument str (metavar "filter" <> help "Comma separated file exts to include")

parseInc =
  flag True False (long "no-increment" <> short 'i' <> help "Whether or not to use existing summaries")

unaryOpts :: Parser Config
unaryOpts =
   Config
    <$> argument str (metavar "ARCHIVE" <> help "The archive root")

-- binaryOpts :: Parser Command
-- binaryOpts = Diff <$>
--   ( Binary
--     <$> argument str (metavar "MASTER" <> help "the master archive")
--     <*> argument str (metavar "CLIENT" <> help "the client archive")
--     <*> parseInc
--   )

commands :: Parser Command
commands = subparser
  ( command "init"
     (info (Init <$> unaryOpts <**> helper)
           (fullDesc <> progDesc "Initialize archive"))
  )
   -- ( command "dupe"
   --   (info (Dupe <$> unaryOpts <**> helper)
   --         (fullDesc <> progDesc "Find duplicates in an archive"))
   -- <> command "diff"
   --   (info (binaryOpts <**> helper)
   --         (fullDesc <> progDesc "Asymmetrical difference between archives")
   --   )
   -- <> command "snap"
   --   (info (Snap <$> unaryOpts <**> helper)
   --         (fullDesc <> progDesc "Summarize archive content")
   --   )
   -- )

-- snap :: (MonadArx m) ⇒ m Snapshot
-- snap = do
--   s ← updateSnapshot
--   p ← snapPath <$> config
--   liftIO $ writeSnapshot p s
--   return s

-- dupe :: (MonadArx m) ⇒ m ()
-- dupe = do
--   s ← updateSnapshot
--   dups ← duplicates
--   forM_ dups $ \ds → do
--     logline (show dups)

run :: Command → IO ()
run (Init c) = void $ arx c (Arx.init :: Arx ())
-- run (Dupe c)   = void $ arx c (dupe :: Arx ())
-- run (Diff (Binary l r i)) = do
--   let lc = Config l i
--   let rc = Config r i

--   left  ← arx lc (snap :: Arx Snapshot)
--   right ← arx rc (snap :: Arx Snapshot)

--   let miss = missing left right

--   logline $ (show $ numUnique left) ++ " unique files in master"
--   logline $ (show $ numUnique right) ++ " unique files in client"

--   putStrLn $ "Missing files (" ++ show (size miss) ++ "):"
--   forM_ miss $ \obj → do
--     logline (show obj)
-- run (Snap c)   = void $ arx c (snap :: Arx Snapshot)

main :: IO ()
main = do
  cmd ← execParser (info commands idm)
  run cmd
