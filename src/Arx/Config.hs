module Arx.Config where

import Data.Default
import Data.Aeson.Types as Aeson
import Control.Lens hiding ((.:))
import System.FilePath

data Settings = Settings {}

instance Default Settings where
  def = Settings {}

data Config = Config
  { _root     :: FilePath
  , _settings :: Settings
  }

makeLenses ''Settings
makeLenses ''Config

instance ToJSON Settings where
  toJSON (Settings {}) = object [] --  "cache" Aeson..= object [ "whitelist" Aeson..= whitelist ]

instance FromJSON Settings where
  parseJSON (Object v) = do return Settings
    -- cacheObj  <- (v Aeson..: "cache") :: Parser Object
    -- whitelist <- cacheObj Aeson..: "whitelist"
    -- return (Settings whitelist)

arxDir :: String
arxDir = ".arx"

settingsFilename :: String
settingsFilename = "config.yaml"

cache :: String
cache = "cache.sqlite"

getArchivePath :: Getter Config FilePath
getArchivePath = to (\c -> c^.root </> arxDir)
  
logPath :: Getter Config FilePath
logPath = getArchivePath . to (\arch -> arch </> "debug")

dbPath :: Getter Config FilePath
dbPath = getArchivePath . to (\arch -> arch </> cache)

getSettingsPath :: Getter Config FilePath
getSettingsPath = getArchivePath . to (\arch -> arch </> settingsFilename)
