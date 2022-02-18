module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens ((^.))

import Data.Default
import Data.List (isPrefixOf)
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
import Arx.Api
import Arx.OnDisk
import Arx.FileUtils
import Arx.FileTree (RootNode(..),findTree)
-- import Arx.Server
-- import Arx.Client
import qualified Arx as Arx

manifestfile = "manifest.arx"

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
  | Status
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
  Manifest <$> option auto (metavar "PATH?" <> value ".")

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
  <> command "status"
     (info (pure Status <**> helper)
           (fullDesc <> progDesc "Check the status of the archive."))
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
  r <- makeAbsolute root
  void $ create (Config r (r </> arxDir) Settings)

run (AddFile f) = do
  f      <- makeAbsolute f
  result <- arx (new f) -- TODO errrs
  case result of
    (Left err) -> putStrLn (show err)
    (Right _ ) -> return ()

run Status = do
  cd <- getCurrentDirectory 
  s <- arx $ do
    sp <- store
    t  <- liftIO $ findTree (const ()) cd (\p i -> not (sp `isPrefixOf` p))
    status (RootNode (takeDirectory cd) t)
  prettyStatus s

-- run Cache = do
--   c ← findArxConfig
--   arx c (Arx.buildCache :: Arx ())

-- run Serve = do
--   c ← findArxConfig
--   server c

-- run (CheckManifest mf) = do


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

main :: IO ()
main = do
  cmd ← execParser (info commands idm)
  run cmd
