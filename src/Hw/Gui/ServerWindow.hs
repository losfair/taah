module Hw.Gui.ServerWindow (St, mkSt, render) where

import Control.Monad.State
import qualified DearImGui as G
import Control.Lens
import Data.IORef
import Hw.Server
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Network.Socket as Sock
import Data.ByteString.Internal (w2c)

data St = St {
  _stCounter :: Int,
  _stListener :: Maybe ServerApi,
  _stListenAddrInput :: IORef String,
  _stListenServiceInput :: IORef String,
  _stLogs :: ![LogEntry]
}

data LogEntry = LogEntry {
  leText :: !String,
  leMsgCharAddr :: !(Maybe Sock.SockAddr)
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
    _stListenServiceInput = listenServiceInput,
    _stLogs = []
  }

render :: (MonadIO m, MonadState St m) => m ()
render = do
  current <- get
  G.text $ "Frame count: " ++ show (view stCounter current)
  case view stListener current of
    Just listener -> do
      G.text $ "Listening on " ++ show (apiListenAddr listener)
      G.separator

      -- Logs
      bufferedMsg <- liftIO $ atomically $ tryReadTQueue $ apiMessageBus listener
      renderLogs bufferedMsg
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

renderLogs :: (MonadIO m, MonadState St m) => Maybe ServerMessage -> m ()
renderLogs msg = do
  current <- get
  let prevLogs = take 100 $ view stLogs current

  -- should we rewrite the last log entry?
  let newLogs = case (prevLogs, msg) of
                  (x:xs, Just (MsgChar addr byte)) | leMsgCharAddr x == Just addr ->
                    x { leText = leText x ++ [w2c byte] } : xs
                  (xs, Just next) -> mkEntry next : xs
                  (xs, Nothing) -> xs
  put $ set stLogs newLogs current

  G.beginChild "ConsoleView"
  liftIO $ mapM_ renderItem (reverse newLogs)
  G.endChild

  return ()

  where
    mkEntry :: ServerMessage -> LogEntry
    mkEntry msg = case msg of
      MsgChar peer ch ->
        LogEntry {
          leText = "[" ++ show peer ++ "] " ++ [w2c ch],
          leMsgCharAddr = Just peer
        }
      _ -> LogEntry { leText = show msg, leMsgCharAddr = Nothing }

    renderItem :: LogEntry -> IO ()
    renderItem entry = do
      G.textWrapped $ leText entry