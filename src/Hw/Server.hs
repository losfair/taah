module Hw.Server (
  generateServer,
  ServerConfig(ServerConfig), cfgListenIP, cfgListenService, cfgAccountDbPath,
  ServerApi, apiMessageBus, apiListenAddr
) where

import Control.Concurrent.STM.TQueue
import qualified Database.SQLite.Simple as DB
import Control.Concurrent
import qualified Network.Socket as Sock
import Control.Exception
import Control.Monad.Managed
import Control.Monad
import Hw.User ( UserInfo(UserInfo) )
import Control.Concurrent.STM
import qualified Network.Socket.ByteString as SockBS
import qualified Data.ByteString as B
import Data.IORef
import Control.Monad.State
import Data.Word
import Control.Lens
import Data.ByteString.Internal (c2w)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

data ServerConfig = ServerConfig {
  cfgListenIP :: String,
  cfgListenService :: String,
  cfgAccountDbPath :: String
}

data ServerApi = ServerApi {
  apiMessageBus :: TQueue ServerMessage,
  apiListenAddr :: Sock.SockAddr
}

data ServiceState = ServiceState {
  _stDbConn :: DB.Connection,
  _stListener :: Sock.Socket,
  _stMessageBus :: TQueue ServerMessage
}

data ServerMessage =
  MsgNewClient Sock.SockAddr 
  | MsgAuthFailed Sock.SockAddr
  | MsgClientAuthenticated Sock.SockAddr UserInfo
  deriving (Show)

data ConnState = ConnState {
  _csRecvBuffer :: B.ByteString
}

$(makeLenses ''ServiceState)
$(makeLenses ''ConnState)

generateServer :: ServerConfig -> IO ServerApi
generateServer config = do
  dbConn <- DB.open $ cfgAccountDbPath config
  resolvedAddr <- resolve
  listener <- listen resolvedAddr
  messageBus <- newTQueueIO
  let st = ServiceState {
    _stDbConn = dbConn,
    _stListener = listener,
    _stMessageBus = messageBus
  }
  forkIO $ runServer st
  pure ServerApi { apiMessageBus = messageBus, apiListenAddr = Sock.addrAddress resolvedAddr }
  where
    resolve = do
      let hints = Sock.defaultHints { Sock.addrFlags = [Sock.AI_PASSIVE], Sock.addrSocketType = Sock.Stream}
      head <$> Sock.getAddrInfo (Just hints) (Just $ cfgListenIP config) (Just $ cfgListenService config)
    listen addr = do
      let sock = Sock.socket (Sock.addrFamily addr) (Sock.addrSocketType addr) (Sock.addrProtocol addr)
      bracketOnError sock Sock.close $ \sock -> do
        Sock.setSocketOption sock Sock.ReuseAddr 1
        Sock.withFdSocket sock Sock.setCloseOnExecIfNeeded
        Sock.bind sock $ Sock.addrAddress addr
        Sock.listen sock 1024
        return sock

runServer :: ServiceState -> IO ()
runServer st = forever do
  (conn, peer) <- onException (Sock.accept $ view stListener st) (Sock.close $ view stListener st)
  let connState = ConnState { _csRecvBuffer = B.empty }
  forkIO $ finally (handle (onConnException conn peer) (evalStateT (handleConnection st conn peer) connState)) (Sock.close conn)
  return ()

onConnException :: Sock.Socket -> Sock.SockAddr -> SomeException -> IO ()
onConnException conn peer exc = do
  putStrLn $ "Exception when handling connection with " ++ show peer ++ ": " ++ show exc


handleConnection :: (MonadIO m, MonadState ConnState m) => ServiceState -> Sock.Socket -> Sock.SockAddr -> m ()
handleConnection st conn peer = do
  liftIO $ atomically $ writeTQueue (view stMessageBus st) (MsgNewClient peer)

  -- Auth
  liftIO $ SockBS.sendAll conn "Username: "
  username_ <- readBytesUntil conn (c2w '\n')
  liftIO $ SockBS.sendAll conn "Password: "
  password_ <- readBytesUntil conn (c2w '\n')

  let username = T.strip $ decodeUtf8 username_
  let password = T.dropWhileEnd (\x -> x == '\r' || x == '\n') $ decodeUtf8 password_

  result <- liftIO (DB.query
    (view stDbConn st)
    "select password from users where username = ?"
    (DB.Only username) :: IO [DB.Only T.Text])

  -- We are not hashing passwords here - DON'T DO THIS IN A REAL SYSTEM!
  let ok = case result of
            (DB.Only t):_ -> t == password
            [] -> False
  if ok then do
    liftIO $ atomically $ writeTQueue (view stMessageBus st) (MsgClientAuthenticated peer UserInfo {})
    enterSession st conn peer
  else do
    liftIO $ atomically $ writeTQueue (view stMessageBus st) (MsgAuthFailed peer)
    liftIO $ SockBS.sendAll conn "Authentication failed.\r\n"

enterSession :: (MonadIO m, MonadState ConnState m) => ServiceState -> Sock.Socket -> Sock.SockAddr -> m ()
enterSession st conn peer = do
  liftIO $ SockBS.sendAll conn "Hello!\r\n"

readBytesUntil :: (MonadIO m, MonadState ConnState m) => Sock.Socket -> Word8 -> m B.ByteString
readBytesUntil conn terminator = do
  initBuf <- get
  buffers <- search (view csRecvBuffer initBuf) (SockBS.recv conn 4096)
  return $ B.intercalate B.empty buffers
  where
    search :: (MonadIO m, MonadState ConnState m) => B.ByteString -> IO B.ByteString -> m [B.ByteString]
    search x next = case B.findIndex (== terminator) x of
      Just i_ -> do
        -- Save the remaining bytes into the buffer
        -- Include the terminating byte
        let i = i_ + 1
        when (i < B.length x) do
          get >>= put . set csRecvBuffer (B.drop i x)
        return [B.take i x]
      Nothing -> do
        nextBuf <- liftIO next
        after <- search nextBuf next
        return $ x : after
