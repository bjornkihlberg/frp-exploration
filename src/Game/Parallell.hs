module Game.Parallell where

import Control.Monad.Trans.MSF (MaybeT (..))
import qualified Control.Monad.Trans.MSF as MSF.Trans
import Data.MonadicStreamFunction (MSF, (<<<))
import qualified Data.MonadicStreamFunction as MSF

newtype Time = Time Double
  deriving (Eq, Ord)

lookupES :: Monad m => (Time -> Maybe a) -> MSF (MaybeT m) Time a
lookupES funkalunk = MSF.arrM (MaybeT . pure . funkalunk)

f :: Time -> Double
f (Time t) = t * 2

fS :: Monad m => MSF m Time Double
fS = MSF.arr f

fS' :: Monad m => Time -> MSF m Time Double
fS' t = MSF.arr (const $ f t)

pathS :: Monad m => Time -> MSF (MaybeT m) Time Double
pathS t = fS <<< MSF.Trans.exitWhen (>= t)

game :: Monad m => MSF m Time Double
game = pathS (Time 5) `MSF.Trans.catchMaybe` fS' (Time 5)
