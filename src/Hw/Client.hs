module Hw.Client (
  ClientConfig(ClientConfig, cfgConnectIP, cfgConnectService),
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
import Data.List (foldl')

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
  apiConnectAddr :: Sock.SockAddr,
  apiSocket :: Sock.Socket
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
    (finally (runService service sock) (Sock.close sock))
    (atomically . writeTQueue dataRecv . RecvException . show :: SomeException -> IO ())

  let api = ClientApi {
    apiDataSend = dataSend,
    apiDataRecv = dataRecv,
    apiKillHandle = serviceTid,
    apiConnectAddr = Sock.addrAddress addr,
    apiSocket = sock
  }
  return api
  where
    resolve = do
      let hints = Sock.defaultHints { Sock.addrFlags = [], Sock.addrSocketType = Sock.Stream}
      head <$> Sock.getAddrInfo (Just hints) (Just $ cfgConnectIP cfg) (Just $ cfgConnectService cfg)

closeClient :: ClientApi -> IO ()
closeClient api = do
  killThread $ apiKillHandle api
  -- Interrupt ongoing socket receives.
  void (try $ Sock.shutdown (apiSocket api) Sock.ShutdownBoth :: IO (Either SomeException ()))

tryReadData :: ClientApi -> IO [B.ByteString]
tryReadData api = do
  msgs <- atomically $ flushTQueue $ apiDataRecv api
  case msgs of
    RecvException e:_ -> throwIO $ ClientException e
    _ -> return ()

  let (d, e) = span isData msgs
  case e of
    (RecvException e):_ -> atomically $ unGetTQueue (apiDataRecv api) (RecvException e)
    _ -> return ()
  return $ map unwrapData d
  where
    isData (RecvData _) = True
    isData (RecvException _) = False
    unwrapData (RecvData x) = x

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