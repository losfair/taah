module Hw.Server (
  generateServer,
  ServerConfig(ServerConfig), cfgListenIP, cfgListenService, cfgAccountDbPath,
  ServerApi, apiMessageBus, apiListenAddr
) where

import Control.Concurrent.Chan
import qualified Database.SQLite.Simple as DB
import Control.Concurrent
import qualified Network.Socket as Sock
import Control.Exception
import Control.Monad.Managed
import Control.Monad
import Hw.User

data ServerConfig = ServerConfig {
  cfgListenIP :: String,
  cfgListenService :: String,
  cfgAccountDbPath :: String
}

data ServerApi = ServerApi {
  apiMessageBus :: Chan ServerMessage,
  apiListenAddr :: Sock.SockAddr
}

data ServiceState = ServiceState {
  stDbConn :: DB.Connection,
  stListener :: Sock.Socket,
  stMessageBus :: Chan ServerMessage
}

data ServerMessage =
  MsgNewClient Sock.SockAddr 
  | MsgClientAuthenticated Sock.SockAddr UserInfo
  deriving (Show)
generateServer :: ServerConfig -> IO ServerApi
generateServer config = do
  dbConn <- DB.open $ cfgAccountDbPath config
  resolvedAddr <- resolve
  listener <- listen resolvedAddr
  messageBus <- newChan
  let st = ServiceState {
    stDbConn = dbConn,
    stListener = listener,
    stMessageBus = messageBus
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
  (conn, peer) <- onException (Sock.accept $ stListener st) (Sock.close $ stListener st)
  forkIO $ handleConnection st conn peer
  return ()

handleConnection :: ServiceState -> Sock.Socket -> Sock.SockAddr -> IO ()
handleConnection st conn_ peer = runManaged do
  conn <- managed (\f -> f conn_)
  liftIO $ writeChan (stMessageBus st) (MsgNewClient peer)
