{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Arx.Client where

import GHC.Generics
import System.Exit

import Data.Text
import Data.Aeson hiding (json)

import Control.Monad
import Control.Monad.Reader

import Arx
import Arx.Archive
import Arx.Config
import Arx.Monad

import Database.Persist
import Network.HTTP.Req

type Digest = String

type DigestsReqs = [PlainObject]
type DigestsResp = [(FilePath, [FilePath])]
data Client = Client {
  hasDigest :: DigestsReqs → IO DigestsResp
  }

-- local clients
localClient :: Config → Client
localClient c = Client { hasDigest = mapM hasDig }
  where
    hasDig (Plain fp dig) = do
      arx c $ (do
        objs ← checkDig dig
        return $ (fp, fmap (objectPath . entityVal) objs) :: Arx _)

data RemoteConfig = Remote
  { _addr :: String
  , _port :: Int
  }

remoteClient :: RemoteConfig → Client
remoteClient Remote{..} = Client { hasDigest = hasDig }
  where
    hasDig :: DigestsReqs → IO DigestsResp
    hasDig digs = do
      r ← runReq defaultHttpConfig $ req POST (http $ pack _addr)
        (ReqBodyJson $ toJSON digs)
        jsonResponse
        (port _port)
      let resp :: Result DigestsResp = fromJSON $ responseBody r
      case resp of
        Success r → return r
        Error msg → do
          putStrLn $ "Remote client failed: " <> msg
          exitFailure

