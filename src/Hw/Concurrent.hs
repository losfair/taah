module Hw.Concurrent (Signal, mkSignal, sendKillSignal, watchSignal) where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Control.Monad

newtype Signal = Signal (TQueue Bool)

mkSignal :: IO Signal
mkSignal = Signal <$> newTQueueIO

sendKillSignal :: Signal -> IO ()
sendKillSignal (Signal q) = atomically $ writeTQueue q True

watchSignal :: Signal -> IO () -> IO Bool
watchSignal (Signal q) action = do
  tid <- forkIO (finally action $ atomically $ writeTQueue q False)
  killed <- atomically $ readTQueue q
  when killed $ killThread tid
  return killed
