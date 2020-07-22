module Arx.Types where

import Data.Default
import Data.UnixTime
import Data.Text

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

type Arx = ReaderT (Config, SqlBackend) (LoggingT IO)

-- Implementation of MonadArx
-- that uses an sqlite file as a cache.
instance MonadArx Arx where
  config     = fst <$> ask

  runQuery a = do
    backend ← snd <$> ask
    runReaderT a backend
    
  runArx c m = do
    withSqliteConn (pack $ _root c) (\backend → runReaderT m (c, backend))
    
