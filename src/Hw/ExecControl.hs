module Hw.ExecControl (runCommand) where

import qualified Data.Text as T
import qualified Data.ByteString as B
import Control.Exception
import Control.Monad.State (MonadIO, liftIO, MonadState (get, put), forever, StateT (runStateT), unless, when, evalStateT)
import Data.Word (Word8)
import Data.Char
import qualified System.Process as P
import System.IO (Handle, BufferMode (NoBuffering, LineBuffering), hSetBuffering)
import Control.Concurrent
import Data.IORef
import Hw.Concurrent
import Data.ByteString.Internal (c2w)

newtype ExecException = ExecException String
  deriving (Show)
instance Exception ExecException

data IpcPrims = IpcPrims {
  ipcStdin :: Handle,
  ipcStdout :: Handle
}

runCommand :: (MonadIO m, MonadState s m) => T.Text -> (B.ByteString -> IO ()) -> StateT s IO Word8 -> m ()
runCommand cmd_ streamBack nextByte = do
  let !cmd = T.strip cmd_
  liftIO $ putStrLn $ "Running command: " ++ show cmd

  (creationInfo, ipc) <- liftIO $ prepareProcess $ P.shell $ T.unpack cmd
  (_, _, _, procHandle) <- liftIO $ P.createProcess creationInfo

  signal <- liftIO mkSignal

  -- The back path
  liftIO $ forkIO $ finally (backPath streamBack ipc) $ sendKillSignal signal

  st <- get
  stStore <- liftIO $ newIORef st
  liftIO $ watchSignal signal $ finally (evalStateT (feedInputToProcess ipc nextByte stStore) st) do
    P.terminateProcess procHandle

  -- State writeback
  liftIO (readIORef stStore) >>= put

  return ()
  
  where
    feedInputToProcess :: (MonadIO m, MonadState s m) => IpcPrims -> m Word8 -> IORef s -> m ()
    feedInputToProcess ipc nextByte stStore = do
      byte <- nextByte
      -- Commit state
      get >>= liftIO . writeIORef stStore 
      -- ETX (Ctrl-C)
      unless (byte == 3) do
        unless (byte == c2w '\r') do
          liftIO $ B.hPut (ipcStdin ipc) (B.singleton byte)
        feedInputToProcess ipc nextByte stStore

    backPath :: (B.ByteString -> IO ()) -> IpcPrims -> IO ()
    backPath streamBack prims = do
      d_ <- B.hGetSome (ipcStdout prims) 4096
      let d = B.pack $ concatMap replaceNewline $ B.unpack d_
      unless (B.null d) do
        streamBack d
        backPath streamBack prims

prepareProcess :: P.CreateProcess -> IO (P.CreateProcess, IpcPrims)
prepareProcess p = do
  (stdinR, stdinW) <- P.createPipe
  (stdoutR, stdoutW) <- P.createPipe
  hSetBuffering stdinW NoBuffering
  let cp = p {
    -- P.child_user = Just 65534,
    -- P.child_group = Just 65534,
    P.std_in = P.UseHandle stdinR,
    P.std_out = P.UseHandle stdoutW,
    P.std_err = P.UseHandle stdoutW
  }
  return (cp, IpcPrims { ipcStdin = stdinW, ipcStdout = stdoutR })

replaceNewline :: Word8 -> [Word8]
replaceNewline x = if x == c2w '\n' then [c2w '\r', c2w '\n'] else [x]