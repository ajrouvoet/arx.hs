module Arx.Logging where

import Prelude hiding (putStrLn)

import Control.Lens

import Data.Default
import Data.Text.Lazy
import Data.Text.Lazy.IO
import System.ProgressBar

import Control.Monad.Reader
import Control.Monad.State

import Debug.Trace

data Message = Info Text | Warning Text | Error Text | Silent
  deriving Show

printMessage :: Message -> IO ()
printMessage (Info i)    = putStrLn $ ">\t" <> i
printMessage (Warning w) = putStrLn $ "!\t" <> w
printMessage (Error e)   = putStrLn $ "E\t" <> e
printMessage Silent      = return ()

data Task = Task
  { _description :: Text
  , _runTask     :: IO Message
  }

data RuntimeSt = Rt
  { _tasks   :: [Task]
  , _thelog  :: [Message]
  }

instance Default RuntimeSt where
  def = Rt [] []

makeLenses ''Task
makeLenses ''RuntimeSt

type Runtime =
  ReaderT (ProgressBar Text) (
  StateT RuntimeSt
  IO)

addTask :: Task -> Runtime ()
addTask t = do
  tasks %= (t:)
  pb <- ask
  liftIO $ updateProgress pb $ \case pr@Progress{..} -> pr { progressTodo = progressTodo + 1 }

writelog :: Message -> Runtime ()
writelog m = thelog %= (m:)

setGoal :: Text -> Runtime ()
setGoal t = do
  pb <- ask
  liftIO $ updateProgress pb $ \case pr@Progress{..} -> pr { progressCustom = t }

tick :: Runtime ()
tick = ask >>= \ pb -> liftIO (incProgress pb 1)

step :: Runtime Bool
step = do
  ts <- use tasks
  case ts of
    []   -> return False
    t:ts' -> do
      tasks .= ts'
      setGoal (t ^. description)
      m <- liftIO (t ^. runTask)
      tick
      writelog m
      return True

runAll :: Runtime ()
runAll = do
  b <- step
  when b runAll

goalLabel :: Label Text
goalLabel = Label
  { runLabel = \pr _ -> progressCustom pr }

runtimePbStyle :: Style Text
runtimePbStyle = defStyle { stylePrefix = goalLabel }

execRuntime :: Runtime a -> IO a
execRuntime c = do
  pb <- newProgressBar runtimePbStyle 10.0 (Progress 0 0 "Waiting...")
  evalStateT (runReaderT c pb) def