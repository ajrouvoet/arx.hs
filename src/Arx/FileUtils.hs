module Arx.FileUtils where

import Control.Monad.IO.Class
import System.FilePath
import System.FilePath.Find

regularFilesOf :: (MonadIO m) ⇒ FilePath -> m [FilePath]
regularFilesOf root = do
  files ← liftIO $ find always (fileType ==? RegularFile) root
  return ((\p → normalise (root </> p)) <$> files)

