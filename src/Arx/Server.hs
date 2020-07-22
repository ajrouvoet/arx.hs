{-# LANGUAGE DeriveGeneric #-}
module Arx.Server where

import GHC.Generics
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import Web.Scotty
import Web.Scotty.Trans (ActionT)
import Data.Aeson hiding (json)
import Data.Text

import Database.Persist.Sqlite

import Arx.Config
import Arx
import Arx.Client
import Arx.Archive
import Arx.Config
import qualified Arx as Arx

handler :: Config → PlainObject → ActionT _ IO (FilePath, [FilePath])
handler c (Plain fp dig) = do
  matches ← liftIO $ arx c (Arx.checkDig dig :: Arx _)
  return (fp, fmap (objectPath . entityVal) matches)

app :: Config → ScottyM ()
app c = do
  post "/" $ do
    reqs :: DigestsReqs ← jsonData
    resp ← forM reqs (handler c)

    liftIO $ putStrLn "Handled request"
    json resp

server :: Config → IO ()
server c = scotty 8888 (app c)
