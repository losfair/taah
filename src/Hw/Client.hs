module Hw.Client (
  ClientConfig(cfgConnectIP, cfgConnectService),
  ClientApi,
  generateClient,
  closeClient,
  tryReadData,
  writeData
) where

import qualified Network.Socket as Sock
import Control.Concurrent.STM
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Exception
import Control.Monad (forever, when)
import qualified Network.Socket.ByteString as SockBS
import Hw.Protocol
import Control.Monad.State
import Control.Lens

newtype ClientException = ClientException String
  deriving (Show)

instance Exception ClientException

data ClientConfig = ClientConfig {
  cfgConnectIP :: String,
  cfgConnectService :: String
}

data RecvItem = RecvData B.ByteString | RecvException String

data ClientApi = ClientApi {
  apiDataSend :: TQueue B.ByteString,
  apiDataRecv :: TQueue RecvItem,
  apiKillHandle :: ThreadId,
  apiConnectAddr :: Sock.SockAddr
}

data ClientService = ClientService {
  csDataSend :: TQueue B.ByteString,
  csDataRecv :: TQueue RecvItem
}

newtype RecvStateMachine = RecvStateMachine {
  _tcs :: TelnetControlState
}

$(makeLenses ''RecvStateMachine)

generateClient :: ClientConfig -> IO ClientApi
generateClient cfg = do
  addr <- resolve
  sock <- Sock.socket (Sock.addrFamily addr) (Sock.addrSocketType addr) (Sock.addrProtocol addr)
  Sock.connect sock (Sock.addrAddress addr)

  dataSend <- newTQueueIO
  dataRecv <- newTQueueIO

  let service = ClientService {
    csDataSend = dataSend,
    csDataRecv = dataRecv
  }
  serviceTid <- forkIO $ catch
    (runService service sock)
    (atomically . writeTQueue dataRecv . RecvException . show :: SomeException -> IO ())

  let api = ClientApi {
    apiDataSend = dataSend,
    apiDataRecv = dataRecv,
    apiKillHandle = serviceTid,
    apiConnectAddr = Sock.addrAddress addr
  }
  return api
  where
    resolve = do
      let hints = Sock.defaultHints { Sock.addrFlags = [], Sock.addrSocketType = Sock.Stream}
      head <$> Sock.getAddrInfo (Just hints) (Just $ cfgConnectIP cfg) (Just $ cfgConnectService cfg)

closeClient :: ClientApi -> IO ()
closeClient api = do
  killThread $ apiKillHandle api

tryReadData :: ClientApi -> IO (Maybe B.ByteString)
tryReadData api = do
  msg <- atomically $ tryReadTQueue $ apiDataRecv api
  case msg of
    Just (RecvData x) -> return $ Just x
    Just (RecvException e) -> throwIO $ ClientException e
    Nothing -> return Nothing

writeData :: ClientApi -> B.ByteString -> IO ()
writeData api buf = atomically $ writeTQueue (apiDataSend api) buf

runService :: ClientService -> Sock.Socket -> IO ()
runService svc conn = do
  recvCh <- newTQueueIO
  bracket (forkIO $ receiveIt recvCh) killThread $ \_ -> forever do
    event <- atomically $ orElse (Left <$> readTQueue recvCh) (Right <$> readTQueue (csDataSend svc))
    case event of
      -- Recv exception
      Left (Left e) -> do
        throwIO $ ClientException e
      -- Recv data
      Left (Right buf) -> do
        atomically $ writeTQueue (csDataRecv svc) (RecvData buf)
      -- Send data
      Right buf -> do
        SockBS.sendAll conn buf

  where
    receiveIt ch = handle (atomically . writeTQueue ch . Left . show :: SomeException -> IO ()) do
      let st = RecvStateMachine { _tcs = TcsIdle }
      evalStateT (forever $ receiveOnce ch) st

    receiveOnce :: (MonadIO m, MonadState RecvStateMachine m) => TQueue (Either String B.ByteString) -> m ()
    receiveOnce ch = do
      buf_ <- liftIO $ SockBS.recv conn 4096
      when (B.null buf_) do
        liftIO $ throwIO $ ClientException "EOF"
      buf <- B.pack <$> filterM (filterTelnetControl tcs) (B.unpack buf_)
      unless (B.null buf) do
        liftIO $ atomically $ writeTQueue ch (Right buf)