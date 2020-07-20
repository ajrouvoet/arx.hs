{-# LANGUAGE DeriveGeneric #-}
module Arx.Server where

import GHC.Generics
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import Web.Scotty
import Data.Aeson hiding (json)

import Database.Persist.Sqlite

import Arx.Config
import Arx
import Arx.Archive
import Arx.Config
import qualified Arx as Arx

data Req
  = Has { digest :: String }
  deriving (Generic, Show)

data Resp
  = Here    { matches :: [FilePath] }
  | Nothere
  deriving (Generic, Show)

instance FromJSON Req
instance ToJSON Req

instance FromJSON Resp
instance ToJSON Resp

app :: Config → ScottyM ()
app c = do
  post "/" $ do
    reqs :: [Req] ← jsonData
    resp ← forM reqs $ \(Has dig) → do
      matches ← liftIO $ runStderrLoggingT $ arx c (Arx.checkDig dig :: Arx _)
      if matches == []
        then return Nothere
        else return (Here (objectPath . entityVal <$> matches))

    json resp

server :: Config → IO ()
server c = scotty 8888 (app c)
