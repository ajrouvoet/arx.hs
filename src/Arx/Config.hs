module Arx.Config where

import Control.Lens
import System.FilePath

data Config
  = Config { _root :: FilePath }

makeLenses ''Config

arxDir :: String
arxDir = ".arx"

cache :: String
cache = "cache.sqlite"

getArchivePath :: Getter Config FilePath
getArchivePath = to (\c -> c^.root </> arxDir)

dbPath :: Getter Config FilePath
dbPath = getArchivePath . to (\arch -> arch </> cache)
