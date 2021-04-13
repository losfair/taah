module Hw.Gui.ServerWindow (St, mkSt, render) where

import Control.Monad.State
import qualified DearImGui as G
import qualified DearImGui.Raw as GR
import Control.Lens
import Data.IORef
import Hw.Server
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Network.Socket as Sock
import Data.ByteString.Internal (w2c)
import Hw.TimeIt (timeItNamed)
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Text.Encoding

data St = St {
  _stListener :: Maybe ServerApi,
  _stListenAddrInput :: IORef String,
  _stListenServiceInput :: IORef String,
  _stLogs :: ![LogEntry],
  _stLogCache :: B.ByteString
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
    _stListener = Nothing,
    _stListenAddrInput = listenAddrInput,
    _stListenServiceInput = listenServiceInput,
    _stLogs = [],
    _stLogCache = B.empty
  }

render :: (MonadIO m, MonadState St m) => m ()
render = do
  current <- get
  case view stListener current of
    Just listener -> do
      G.text $ "Listening on " ++ show (apiListenAddr listener)
      G.spacing
      G.separator
      G.spacing

      -- Logs
      bufferedMsgs <- liftIO $ atomically $ flushTQueue $ apiMessageBus listener
      renderLogs bufferedMsgs
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
  pure ()

renderLogs :: (MonadIO m, MonadState St m) => [ServerMessage] -> m ()
renderLogs msgs = do
  let appendLogs = reverse $ map mkEntry msgs
  unless (null appendLogs) $ timeItNamed "server_renderLogs" do
    current <- get
    let prevLogs = take 100 $ view stLogs current
    let newLogs = foldLogs $ appendLogs ++ prevLogs
    get >>= put . set stLogs newLogs
    get >>= put . set stLogCache (encodeUtf8 $ T.intercalate "\n" $ map renderItem $ reverse newLogs)

  current <- get
  G.beginChild "ConsoleView"
  let logCache = view stLogCache current
  unless (B.null logCache) do
    liftIO $ BU.unsafeUseAsCStringLen logCache GR.textUnformatted
  unless (null msgs) do
    liftIO $ G.setScrollHereY 1.0
  G.endChild

  return ()

  where
    mkEntry :: ServerMessage -> LogEntry
    mkEntry msg = case msg of
      MsgChar peer ch ->
        LogEntry {
          leText = [w2c ch],
          leMsgCharAddr = Just peer
        }
      _ -> LogEntry { leText = show msg, leMsgCharAddr = Nothing }

    renderItem :: LogEntry -> T.Text
    renderItem entry = case leMsgCharAddr entry of
      Just x -> T.pack $ "[" ++ show x ++ "] " ++ reverse (leText entry)
      Nothing -> T.pack $ leText entry

    foldLogs :: [LogEntry] -> [LogEntry]
    foldLogs !logs = case logs of
      a:b:xs |
        isJust (leMsgCharAddr a)
          && leMsgCharAddr a == leMsgCharAddr b
          && head (leText b) /= '\n'
          && length (leText a) + length (leText b) <= 256 ->
        foldLogs $ b { leText = leText a ++ leText b } : xs 
      a:xs -> a : foldLogs xs
      [] -> []
