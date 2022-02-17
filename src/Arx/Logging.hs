module Arx.Logging where

-- import System.ProgressBar

-- import Control.Monad.Reader
-- import Control.Monad.Writer

-- data Message = Info Text | Warning Text | Error Text

-- class Logger l where

--   log :: Message -> l ()

--   -- Report what we're working on and how many.
--   workOn   :: Text -> Int -> l ()
--   -- finish some units of work and update the goal.
--   progress :: Text -> Int -> l ()


-- type Log     = [Message]
-- type IOBar a = ReaderT (ProgressBar Text) (WriterT Log IO a)

-- instance Logger IOBar where

--   log = tell

--   workOn lbl work = do
--     pb <- ask
--     liftIO $ updateProgress
--       (\pr -> pr
--         { progressTodo   = progressTodo pr + work
--         , progressCustom = lbl
--         }) pb

--   progress lbl work = do
--     pb <- ask
--     liftIO $ updateProgress
--       (\pr -> pr
--         { progressDone   = progressDone pr + work
--         , progressCustom = lbl
--         }) pb
