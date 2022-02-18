module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens ((^.), use)

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


import Arx
import Arx.Api
import Arx.OnDisk
import Arx.FileUtils ( regularFilesOf )
import Arx.FileTree (RootNode(..),findTree)
-- import Arx.Server
-- import Arx.Client
import qualified Arx as Arx
import Arx.Logging

import Debug.Trace

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
  | AddFile [FilePath]
  | Status
  | Manifest FilePath
  | CheckManifest FilePath
  -- = Init FilePath
  -- | Cache
  -- | Contains { remote :: ClientConf }
  -- | Serve

addOpts :: Parser [FilePath]
addOpts =
  some $ argument str (metavar "FILES" <> help "Files to be added to the archive.")

createOpts :: Parser FilePath
createOpts =
  argument str (metavar "ROOT" <> help "Create the archive, rooted in ROOT.")

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

run :: Command → IO ()

run (Create root) = do
  r <- makeAbsolute root
  void $ create (Config r (r </> arxDir) Settings)

run (AddFile fs) = do
  execRuntime $ do
    forM_ fs $ \f -> do
      f' <- liftIO $ makeAbsolute f
      addTask $ Task (pack f) $ arx (do -- TODO make more efficient by getting the common parts of arx out
        m <- new f'
        case m of
          Left err -> return $ Error ((pack f) <> ": " <> (pack $ show err))
          Right _  -> return Silent)
    runAll
    l <- use thelog
    forM_ l $ \m -> do
      liftIO $ putStrLn $ show m

run Status = do
  cd <- getCurrentDirectory
  s <- arx $ do
    sp <- store
    t  <- liftIO $ findTree (const ()) cd (\p i -> not (sp `isPrefixOf` p))
    status (RootNode (takeDirectory cd) t)
  prettyStatus s

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
