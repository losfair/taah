module Hw.Gui.ClientWindow (St, mkSt, render) where

import Control.Monad.State
import qualified DearImGui as G
import Control.Lens
import Data.IORef
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Network.Socket as Sock
import Data.ByteString.Internal (w2c, c2w)
import Hw.TimeIt (timeItNamed)
import Data.Maybe
import Hw.Client
import qualified Data.ByteString as B
import Data.Text.Encoding (decodeUtf8, encodeUtf8, decodeUtf8', decodeUtf8With)
import Control.Exception

data St = St {
  _stConnectAddrInput :: IORef String,
  _stConnectServiceInput :: IORef String,
  _stXmitBoxInput :: IORef String,
  _stClient :: Maybe ClientApi,
  _stLogs :: ![T.Text],
  _stXmitLastLen :: Int
}

$(makeLenses ''St)

mkSt :: MonadIO m => m St
mkSt = do
  connectAddrInput <- liftIO $ newIORef "127.0.0.1"
  connectServiceInput <- liftIO $ newIORef "2233"
  xmitBoxInput <- liftIO $ newIORef ""
  return St {
    _stConnectAddrInput = connectAddrInput,
    _stConnectServiceInput = connectServiceInput,
    _stXmitBoxInput = xmitBoxInput,
    _stClient = Nothing,
    _stLogs = [],
    _stXmitLastLen = 0
  }

render :: (MonadIO m, MonadState St m) => m ()
render = do
  current <- get
  case view stClient current of
    Just client -> do
      wantsDisconnect <- G.button "Disconnect"
      enterPressed <- G.inputTextEnterReturnsTrue "Xmit" (view stXmitBoxInput current) 256

      xmitBox <- liftIO $ readIORef (view stXmitBoxInput current)
      let lastLen = view stXmitLastLen current
      let !thisLen = length xmitBox
      put $ set stXmitLastLen thisLen current
      let added = drop lastLen xmitBox
      unless (null added) do
        liftIO $ writeData client $ encodeUtf8 $ T.pack added
      when enterPressed do
        liftIO $ writeIORef (view stXmitBoxInput current) ""
        put $ set stXmitLastLen 0 current
        liftIO $ writeData client $ B.pack [c2w '\r', 0]

      msgs_ <- liftIO (try $ tryReadData client :: IO (Either SomeException [B.ByteString]))
      msgs <- case msgs_ of
        Left e -> do
          -- We cannot throw here since our new state will be discarded
          closeIt
          return [T.pack $ show e]
        Right x -> return $ map (decodeUtf8With (\_ _ -> Just '�')) x
      renderLogs msgs
      when wantsDisconnect closeIt
      where
        closeIt = do
          current <- get
          liftIO $ closeClient client
          put $ set stClient Nothing current
    Nothing -> do
      G.inputText "Connect address" (view stConnectAddrInput current) 256
      G.inputText "Service/port" (view stConnectServiceInput current) 32
      wantsConnect <- G.button "Connect"
      renderLogs []
      when wantsConnect do
        ip <- liftIO $ readIORef (view stConnectAddrInput current)
        service <- liftIO $ readIORef (view stConnectServiceInput current)
        client <- liftIO $ generateClient ClientConfig { cfgConnectIP = ip, cfgConnectService = service }

        -- Flush state & set client
        liftIO $ writeIORef (view stXmitBoxInput current) ""
        put $
          set stXmitLastLen 0 $
          set stLogs [] $
          set stClient (Just client)
          current
        return ()
      
  return ()

renderLogs :: (MonadIO m, MonadState St m) => [T.Text] -> m ()
renderLogs msgs = do
  let appendLogs = reverse msgs
  unless (null appendLogs) $ timeItNamed "client_renderLogs" do
    current <- get
    let prevLogs = take 100 $ view stLogs current
    let newLogs = appendLogs ++ prevLogs
    put $ set stLogs newLogs current

  current <- get
  G.beginChild "ConsoleView"
  liftIO $ mapM_ renderItem (reverse $ view stLogs current)
  unless (null msgs) do
    liftIO $ G.setScrollHereY 1.0
  G.endChild

  return ()

  where
    renderItem :: T.Text -> IO ()
    renderItem text = G.textWrapped $ T.unpack text