{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE UndecidableInstances       #-}

module Arx.Archive where

import GHC.Generics

import Data.Time
import System.FilePath
import Data.Aeson hiding (Object)

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Snap
    archive  String
    finished UTCTime
    deriving Show
Object
    snap    SnapId
    path    String
    digest  String
    UniquePath snap path
    deriving Show Eq
|]


data PlainObject = Plain
  { path    :: String
  , digest  :: String
  } deriving (Generic)

instance ToJSON PlainObject
instance FromJSON PlainObject

class ArxCache m where

  hasDigest :: String → m [FilePath]
  execCache :: m a → IO a
