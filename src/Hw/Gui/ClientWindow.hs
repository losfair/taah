module Hw.Gui.ClientWindow (St, mkSt, render) where

import Control.Monad.State
import qualified DearImGui as G
import Control.Lens
import Data.IORef
import Hw.Server
import Control.Concurrent.STM
import qualified Data.Text as T
import qualified Network.Socket as Sock
import Data.ByteString.Internal (w2c)
import Hw.TimeIt (timeItNamed)
import Data.Maybe

data St = St {
  _stConnectAddrInput :: IORef String,
  _stConnectServiceInput :: IORef String
}

$(makeLenses ''St)

mkSt :: MonadIO m => m St
mkSt = do
  connectAddrInput <- liftIO $ newIORef "127.0.0.1"
  connectServiceInput <- liftIO $ newIORef "2233"
  return St {
    _stConnectAddrInput = connectAddrInput,
    _stConnectServiceInput = connectServiceInput
  }

render :: (MonadIO m, MonadState St m) => m ()
render = do
  return ()
