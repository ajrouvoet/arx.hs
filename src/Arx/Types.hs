module Arx.Types where

import Data.Default
import Data.UnixTime

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger

import System.Posix.Files as Posix
import System.Posix.Types

import Database.Persist.Sqlite

import Arx.Config
import Arx.Archive
import Arx.Monad

type Arx = ReaderT Config (LoggingT IO)

instance MonadArx Arx where
  config     = ask
  runArx c m = do
    runReaderT m c
