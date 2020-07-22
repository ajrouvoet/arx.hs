{-# LANGUAGE DeriveGeneric #-}
module Arx.Server where

import GHC.Generics
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import Web.Scotty
import Web.Scotty.Trans (ActionT)
import Data.Aeson hiding (json)

import Database.Persist.Sqlite

import Arx.Config
import Arx
import Arx.Client
import Arx.Archive
import Arx.Config
import qualified Arx as Arx

-- Handle root requests
handler :: Config → PlainObject → ActionT _ IO (FilePath, [FilePath])
handler c (Plain fp dig) = do
  matches ← liftIO $ arx c (Arx.checkDig dig :: Arx _)
  return (fp, fmap (objectPath . entityVal) matches)

-- The REST API of the server
app :: Config → ScottyM ()
app c = do

  -- root path
  post "/" $ do
    reqs :: DigestsReqs ← jsonData
    liftIO $ putStrLn $ "[arx:serve] Decode request for "
      <> show (length reqs)
      <> " digests"
    resp ← forM reqs (handler c)

    liftIO $ putStrLn "Handled request"
    json resp

-- Entrypoint of the server.
server :: Config → IO ()
server c = scotty 8888 (app c)
