{-# LANGUAGE PartialTypeSignatures #-}
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
import qualified Arx as Arx

data Command
  = Init Config
  | Cache
  | Contains FilePath

initOpts :: Parser Config
initOpts =
   Config
    <$> argument str (metavar "ARCHIVE" <> help "The archive root")

fileOpt :: Parser String
fileOpt = argument str (metavar "PATH" <> help "File path")

commands :: Parser Command
commands = subparser
  (  command "init"
     (info (Init <$> initOpts <**> helper)
           (fullDesc <> progDesc "Initialize archive"))
  <> command "cache"
     (info (pure Cache)
           (fullDesc <> progDesc "Update the archive cache"))
  <> command "has"
     (info (Contains <$> fileOpt <**> helper)
           (fullDesc <> progDesc "Check if the "))
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
        then return $ Config p
        else
          if p == "/"
          then exitFailure
          else findRoot (takeDirectory p)

run :: Command → IO ()
run (Init c)  = void $ runStderrLoggingT $ arx c (Arx.init :: Arx ())
run Cache = do
  c ← findArxConfig
  runStderrLoggingT $ arx c (Arx.buildCache :: Arx ())
run (Contains f) = do
  c  ← findArxConfig
  f' ← makeAbsolute f
  matches ← runStderrLoggingT $ arx c (Arx.checkFile f' :: Arx _)

  if matches == []
    then do
      putStrLn "No matches found"
      exitFailure
    else do
      putStrLn "Found the following matches:"
      forM_ matches $ \eObj → do
        putStrLn ("> " ++ (objectPath $ entityVal eObj))

main :: IO ()
main = do
  cmd ← execParser (info commands idm)
  run cmd
