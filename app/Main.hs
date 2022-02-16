module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens ((^.))

import Data.Default
import Data.Char (toLower)
import Data.Yaml (decodeFileThrow)
import Data.Hashable
import Data.Map.Strict as Map
import Data.ByteArray ()
import Data.Text.Lazy (pack)

import System.IO
import System.FilePath
import System.FilePath.Find as Find
import System.Directory
import System.Posix.Files as Posix
import System.Exit
import System.ProgressBar
import Options.Applicative

import Database.Persist.Sqlite

import Control.Monad.Logger

import Debug.Trace

import Arx
import Arx.FileUtils
-- import Arx.Server
-- import Arx.Client
import qualified Arx as Arx

manifestfile = "manifest.arx"

arxDir = ".arx"

currentLabel :: Label String
currentLabel = Label {
  runLabel = \pr _ -> pack $ progressCustom pr
  }

myStyle :: Style String
myStyle = defStyle { stylePrefix = currentLabel }

-- type ClientConf = Maybe RemoteConfig

data Command
  = Create FilePath
  | AddFile FilePath
  | Manifest FilePath
  | CheckManifest FilePath
  -- = Init FilePath
  -- | Cache
  -- | Contains { remote :: ClientConf }
  -- | Serve

addOpts :: Parser FilePath
addOpts =
  argument str (metavar "FILE" <> help "File to be added to the archive.")

createOpts :: Parser FilePath
createOpts =
  argument str (metavar "ROOT" <> help "Create the archive, rooted in ROOT.")

-- containOpts :: Parser Command
-- containOpts =
--   Contains <$> (optional $ (\ad → Remote ad 8888) <$> (strOption
--                 (long "remote" <> short 'r'
--                 <> metavar "REMOTE"
--                 <> help "remote Arx server addr")))

-- fileOpt :: Parser String
-- fileOpt = argument str (metavar "PATH" <> help "File path")

manifestOpts :: Parser Command
manifestOpts =
  Manifest <$> (option auto (metavar "PATH?" <> value "."))

checkOpts :: Parser FilePath
checkOpts =
  argument str (metavar "MANIFEST" <> help "Check MANIFEST against archive.")

commands :: Parser Command
commands = subparser
  (  command "create"
     (info (Create <$> createOpts <**> helper)
           (fullDesc <> progDesc "Create new archive"))
  <> command "add"
     (info (AddFile <$> addOpts <**> helper)
           (fullDesc <> progDesc "Add a file to an existing archive"))
  <> command "check"
     (info (CheckManifest <$> checkOpts <**> helper)
           (fullDesc <> progDesc "Check a manifest against the archive"))
  <> command "manifest"
     (info manifestOpts
           (fullDesc <> progDesc "Create a manifest for the PATH (defaults to '.')."))
  )
  -- <> command "serve"
  --    (info (pure Serve)
  --          (fullDesc <> progDesc "Start an archive server"))
  -- <> command "cache"
  --    (info (pure Cache)
  --          (fullDesc <> progDesc "Update the archive cache"))
  -- <> command "has"
  --    (info containOpts
  --          (fullDesc <> progDesc
  --            (  "Read newline separated paths from stdin,"
  --            <> "and check if they are contained in the archive.")))
  -- )

findArxRoot :: IO FilePath
findArxRoot = do
  path ← getCurrentDirectory
  findRoot path
  where
    findRoot :: FilePath → IO FilePath
    findRoot p = do
      let root = (p </> arxDir)
      exists ← doesPathExist root
      if exists
        then do
          return root
        else
          if p == "/"
          then do putStrLn "Not in an Arxive"; exitFailure
          else findRoot (takeDirectory p)

-- getClient :: ClientConf → IO Client
-- getClient Nothing = do
--   c ← findArxConfig
--   putStrLn $ "Local archive at '" <> _root c <> "'"
--   return $ localClient c
-- getClient (Just c) = do
--   putStrLn $ "Remote archive at `" <> show c <> "'"
--   return $ remoteClient c

run :: Command → IO ()

run (Create root) = do
  root <- makeAbsolute root
  void $ create (Config (root </> arxDir) (Settings [root]))

run (AddFile f) = do
  f  <- makeAbsolute f
  r  <- findArxRoot
  result <- runArxOnDisk r (new f) -- TODO errrs
  case result of
    (Left err) -> putStrLn (show err)
    (Right _ ) -> return ()

-- run Cache = do
--   c ← findArxConfig
--   arx c (Arx.buildCache :: Arx ())

-- run Serve = do
--   c ← findArxConfig
--   server c

run (CheckManifest mf) = do

run (Manifest p) = do
  -- first remove the old manifest to prevent reading it while writing it
  exists <- doesPathExist manifestfile
  if exists then removeFile manifestfile else return ()

  -- resources
  files <- regularFilesOf p
  pb    <- newProgressBar myStyle 10 (Progress 0 (length files) "")
  out   <- openFile manifestfile WriteMode

  -- produce the manifest
  forM_ files $ \file -> do
    obj <- getObject file
    hPutStrLn out (obj ^. Arx.path <> "\t" <> obj ^. Arx.digest)
    updateProgress pb (\p -> p{ progressDone = progressDone p + 1
                              , progressCustom = obj ^. Arx.path
                             })

  -- cleanup
  hFlush out
  hClose out

-- run (Contains r) = do
--   putStrLn "[arx:main] Client"
--   Client{..} ← getClient r
--   putStrLn "[arx:main] Parse"
--   paths      ← lines <$> getContents
--   putStrLn "[arx:main] Get digests"
--   objs       ← mapM parseOrGetObject paths
--   putStrLn "[arx:main] Request matches"
--   matches    ← hasDigest objs
--   putStrLn "[arx:main] Outputing"
--   void $ mapM printMatches matches

--   where
--     -- print the matches for a single queried path
--     printMatches (f, matches) = do
--       putStrLn ("? " ++ f)
--       if matches == []
--         then do
--           putStrLn "- No matches found"
--         else do
--           putStrLn "+ Found the following matches:"
--           forM_ matches $ \match → do
--             putStrLn ("\t> " ++ match)
--       putStrLn ""

main :: IO ()
main = do
  cmd ← execParser (info commands idm)
  run cmd
