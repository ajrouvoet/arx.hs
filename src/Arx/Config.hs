module Arx.Config where

import Control.Lens
import System.FilePath

data Config = Config
  { _root        :: FilePath
  }

makeLenses ''Config

getArchivePath :: Getter Config FilePath
getArchivePath = to (\c -> c^.root </> ".arx")

dbPath :: Getter Config FilePath
dbPath = getArchivePath . to (\arch -> arch </> "arx.sqlite")

