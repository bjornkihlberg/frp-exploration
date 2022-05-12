{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Game.Counter where

import Control.Arrow ((<<<))
import Data.MonadicStreamFunction (MSF)
import qualified Data.MonadicStreamFunction as MSF
import qualified Extra.MSF as MSF

data Message = Increment | Decrement

scoreS :: Monad m => MSF m Message Int
scoreS = MSF.arr \case
  Increment -> 1
  Decrement -> (-1)

game :: Monad m => MSF m (Maybe Message) Int
game = MSF.accumulateWith (+) 0 <<< MSF.fromMaybeS 0 <<< MSF.mapMaybeS scoreS
