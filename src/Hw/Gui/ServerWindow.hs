module Hw.Gui.ServerWindow (St, mkSt, render) where

import Control.Monad.State
import qualified DearImGui as G
import Control.Lens
import Data.IORef
import Hw.Server
import Control.Concurrent.STM

data St = St {
  _stCounter :: Int,
  _stListener :: Maybe ServerApi,
  _stListenAddrInput :: IORef String,
  _stListenServiceInput :: IORef String
}

$(makeLenses ''St)

mkSt :: MonadIO m => m St
mkSt = do
  listenAddrInput <- liftIO $ newIORef "127.0.0.1"
  listenServiceInput <- liftIO $ newIORef "2233"
  return St {
    _stCounter = 0,
    _stListener = Nothing,
    _stListenAddrInput = listenAddrInput,
    _stListenServiceInput = listenServiceInput
  }

render :: (MonadIO m, MonadState St m) => m ()
render = do
  current <- get
  G.text $ "Frame count: " ++ show (view stCounter current)
  case view stListener current of
    Just listener -> do
      G.text $ "Listening on " ++ show (apiListenAddr listener)

      bufferedMsg <- liftIO $ atomically $ tryReadTQueue $ apiMessageBus listener
      case bufferedMsg of
        Just x -> do
          liftIO $ print x
        Nothing -> return ()
    Nothing -> do
      G.inputText "Listen address" (view stListenAddrInput current) 256
      G.inputText "Service/port" (view stListenServiceInput current) 32
      wantsListen <- G.button "Listen"
      when wantsListen do
        listenIP <- liftIO $ readIORef (view stListenAddrInput current)
        listenService <- liftIO $ readIORef (view stListenServiceInput current)
        let config = ServerConfig {
          cfgListenIP = listenIP,
          cfgListenService = listenService,
          cfgAccountDbPath = "./users.db"
        }
        listener <- liftIO $ generateServer config
        put $ set stListener (Just listener) current
        return ()
  current <- get
  put $ over stCounter (+ 1) current
  pure ()
