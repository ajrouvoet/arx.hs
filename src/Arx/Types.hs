module Arx.Types where

import Data.Default
import Data.UnixTime

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Logger

import System.Posix.Files as Posix
import System.Posix.Types

import Arx.Config
import Arx.Monad

-- data ArxSt = ArxSt
--   { _snaps       :: Snapshot
--   , _snappedTime :: UnixTime
--   }

-- instance Default ArxSt where
--   def = ArxSt def (UnixTime 0 0)

-- makeLenses ''ArxSt

type Arx = ReaderT Config (LoggingT IO)

instance MonadArx Arx where

  config         = ask

  -- snappedAt      = use snappedTime
  -- snapshot       = use snaps

  -- snapshotModify  f = snaps %= f
  -- snappedAtModify v = snappedTime %= const v

  runArx c m = do
    runReaderT m c
