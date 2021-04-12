module Hw.Protocol (TelnetControlState (TcsIdle), filterTelnetControl) where

import Control.Monad.State
import Data.Word
import Control.Lens

data TelnetControlState =
  TcsIdle
  | TcsGotIac
  | TcsGotOption
  deriving (Show)

filterTelnetControl :: (MonadState s m) => Lens' s TelnetControlState -> Word8 -> m Bool
filterTelnetControl l byte = do
  state <- get
  case view l state of
    TcsIdle ->
      if byte == 255 then do
        put $ set l TcsGotIac state
        return False
      else
        return True
    TcsGotIac -> do
      if byte >= 251 && byte <= 254 then
        put $ set l TcsGotOption state
      else
        put $ set l TcsIdle state
      return False
    TcsGotOption -> do
      put $ set l TcsIdle state
      return False
