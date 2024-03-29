module Hw.Gui.ClientWindow (St, mkSt, render) where

import Control.Monad.State
import qualified DearImGui as G
import qualified DearImGui.Raw as GR
import Control.Lens
import Data.IORef
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Data.Text.Foreign as TF
import qualified Network.Socket as Sock
import Data.ByteString.Internal (w2c, c2w)
import Hw.TimeIt (timeItNamed)
import Data.Maybe
import Hw.Client
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Text.Encoding (decodeUtf8, encodeUtf8, decodeUtf8', decodeUtf8With)
import Control.Exception
import Foreign.Ptr (intPtrToPtr)
import GHC.Exts (coerce)
import GHC.Ptr
import System.IO.Unsafe

data St = St {
  _stConnectAddrInput :: IORef B.ByteString,
  _stConnectServiceInput :: IORef B.ByteString,
  _stXmitBoxInput :: IORef B.ByteString,
  _stClient :: Maybe ClientApi,
  _stLogCache :: B.ByteString,
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
    _stLogCache = B.empty,
    _stXmitLastLen = 0
  }

render :: (MonadIO m, MonadState St m) => m ()
render = do
  current <- get
  case view stClient current of
    Just client -> do
      wantsDisconnect <- GR.button $ Ptr "Disconnect"#
      enterPressed <- G.inputTextB "Xmit" (view stXmitBoxInput current) 256 True

      xmitBox <- liftIO $ readIORef (view stXmitBoxInput current)
      let lastLen = view stXmitLastLen current
      let !thisLen = B.length xmitBox
      put $ set stXmitLastLen thisLen current
      let added = B.drop lastLen xmitBox
      unless (B.null added) do
        liftIO $ writeData client added
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
      G.inputTextB "Connect address" (view stConnectAddrInput current) 128 False
      G.inputTextB "Service/port" (view stConnectServiceInput current) 32 False
      wantsConnect <- GR.button $ Ptr "Connect"#
      renderLogs []
      when wantsConnect do
        ip <- liftIO $ T.unpack . decodeUtf8 <$> readIORef (view stConnectAddrInput current)
        service <- liftIO $ T.unpack . decodeUtf8 <$> readIORef (view stConnectServiceInput current)
        client <- liftIO $ generateClient ClientConfig { cfgConnectIP = ip, cfgConnectService = service }

        -- Flush state & set client
        liftIO $ writeIORef (view stXmitBoxInput current) ""
        put $
          set stXmitLastLen 0 $
          set stLogCache B.empty $
          set stClient (Just client)
          current
        return ()
      
  return ()

renderLogs :: (MonadIO m, MonadState St m) => [T.Text] -> m ()
renderLogs msgs = do
  unless (null msgs) $ timeItNamed "client_renderLogs" do
    current <- get
    let cache = genCache (view stLogCache current) msgs
    put $
      set stLogCache cache
      current

  G.spacing
  G.separator
  G.spacing

  current <- get
  GR.beginChild $ Ptr "ConsoleView"#
  let logCache = view stLogCache current
  unless (B.null logCache) do
    liftIO $ renderItem $ view stLogCache current
  unless (null msgs) do
    liftIO $ G.setScrollHereY 1.0
  G.endChild

  return ()

  where
    genCache :: B.ByteString -> [T.Text] -> B.ByteString
    genCache prev new = encodeUtf8 $ T.takeEnd 65536 $ T.concat (decodeUtf8 prev : new)

    renderItem :: B.ByteString -> IO ()
    renderItem text = BU.unsafeUseAsCStringLen text GR.textUnformatted
