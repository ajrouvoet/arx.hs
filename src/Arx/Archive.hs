{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}

module Arx.Archive where

-- The archive database

import Data.Time

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import Crypto.Hash as Hash

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Snap
    snapshot String
    finished UTCTime
    deriving Show
Object
    snap    SnapId
    path    String
    digest  String
    UniquePath snap path
    deriving Show
|]


data PlainObject = Plain
  { path   :: String
  , digest :: Digest SHA1
  }
