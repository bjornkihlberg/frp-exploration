{-# LANGUAGE BlockArguments #-}

module Extra.MSF where

import qualified Data.Maybe as Maybe
import Data.MonadicStreamFunction (MSF)
import qualified Data.MonadicStreamFunction as MSF
import qualified Data.MonadicStreamFunction.InternalCore as MSF.InternalCore (MSF (..))

fromMaybeS :: Monad m => a -> MSF m (Maybe a) a
fromMaybeS = MSF.arr . Maybe.fromMaybe

nswitch :: Monad m => MSF m a (b, Maybe c) -> (c -> MSF m a b) -> MSF m a b
nswitch sf sfC = MSF.InternalCore.MSF \a -> do
  ((b, mc), ct) <- MSF.InternalCore.unMSF sf a
  pure (b, maybe (nswitch ct sfC) sfC mc)
