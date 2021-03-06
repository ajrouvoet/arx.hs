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
import System.Exit
import Options.Applicative

import Database.Persist.Sqlite

import Control.Monad.Logger

import Debug.Trace

import Arx
import Arx.Archive
import Arx.Config
import Arx.Server
import Arx.Client
import qualified Arx as Arx

type ClientConf = Maybe RemoteConfig

data Command
  = Init Config
  | Cache
  | Contains { remote :: ClientConf }
  | Serve

initOpts :: Parser Config
initOpts =
   Config
    <$> argument str (metavar "ARCHIVE" <> help "The archive root")

containOpts :: Parser Command
containOpts =
 Contains <$> (optional $ (\ad → Remote ad 8888) <$> (strOption
                (long "remote" <> short 'r'
                <> metavar "REMOTE"
                <> help "remote Arx server addr")))

fileOpt :: Parser String
fileOpt = argument str (metavar "PATH" <> help "File path")

commands :: Parser Command
commands = subparser
  (  command "init"
     (info (Init <$> initOpts <**> helper)
           (fullDesc <> progDesc "Initialize archive"))
  <> command "serve"
     (info (pure Serve)
           (fullDesc <> progDesc "Start an archive server"))
  <> command "cache"
     (info (pure Cache)
           (fullDesc <> progDesc "Update the archive cache"))
  <> command "has"
     (info containOpts
           (fullDesc <> progDesc
             (  "Read newline separated paths from stdin,"
             ++ "and check if they are contained in the archive.")))
  )

findArxConfig :: IO Config
findArxConfig = do
  path ← getCurrentDirectory
  findRoot path
  where
    findRoot :: FilePath → IO Config
    findRoot p = do
      let root = (p </> arxDir)
      exists ← doesPathExist root
      if exists
        then do
          return $ Config p
        else
          if p == "/"
          then do putStrLn "Not in an Arx repository"; exitFailure
          else findRoot (takeDirectory p)

getClient :: ClientConf → IO Client
getClient Nothing = do
  c ← findArxConfig
  putStrLn $ "Local archive at '" <> _root c <> "'"
  return $ localClient c
getClient (Just c) = do
  putStrLn $ "Remote archive at `" <> show c <> "'"
  return $ remoteClient c

run :: Command → IO ()

run (Init c) = do
  void $ arx c (Arx.init :: Arx ())

run Cache = do
  c ← findArxConfig
  arx c (Arx.buildCache :: Arx ())

run Serve = do
  c ← findArxConfig
  server c

run (Contains r) = do
  putStrLn "[arx:main] Client"
  Client{..} ← getClient r
  putStrLn "[arx:main] Parse"
  paths      ← lines <$> getContents
  putStrLn "[arx:main] Get digests"
  objs       ← mapM parseOrGetObject paths
  putStrLn "[arx:main] Request matches"
  matches    ← hasDigest objs
  putStrLn "[arx:main] Outputing"
  void $ mapM printMatches matches

  where
    -- print the matches for a single queried path
    printMatches (f, matches) = do
      putStrLn ("? " ++ f)
      if matches == []
        then do
          putStrLn "- No matches found"
        else do
          putStrLn "+ Found the following matches:"
          forM_ matches $ \match → do
            putStrLn ("\t> " ++ match)
      putStrLn ""

main :: IO ()
main = do
  cmd ← execParser (info commands idm)
  run cmd
