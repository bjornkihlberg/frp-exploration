{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Game.Bouncer where

import qualified Control.Monad.Trans.MSF as MSF.Trans
import Data.Coerce (coerce)
import Data.MonadicStreamFunction (MSF, (<<<))
import qualified Data.MonadicStreamFunction as MSF

newtype Position = Position Double
  deriving (Eq, Num, Fractional)

instance Show Position where
  show = coerce @(Double -> String) show

newtype Velocity = Velocity Double
  deriving (Num)

newtype Acceleration = Acceleration Double
  deriving (Num)

g :: Acceleration
g = -1

newtype Time = Time Double
  deriving (Eq, Ord, Num, Fractional)

fallingPosition :: Position -> Velocity -> Time -> Position
fallingPosition (Position p0) (Velocity v0) (Time t) = Position $ p0 + v0 * t + coerce g * t ^ (2 :: Int) / 2

fallingVelocity :: Time -> Velocity -> Velocity
fallingVelocity (Time t) (Velocity v0) = Velocity $ v0 + coerce g * t

fallingPositionS :: Monad m => Time -> Position -> Velocity -> MSF m Time Position
fallingPositionS t0 p0 v0 = MSF.arr $ fallingPosition p0 v0 . subtract t0

fallingPathS :: Monad m => (Time, Time) -> Position -> Velocity -> MSF (MSF.Trans.MaybeT m) Time Position
fallingPathS (t0, t) p0 v0 = fallingPositionS t0 p0 v0 <<< MSF.Trans.exitWhen (>= t)

game :: forall m. Monad m => MSF m Time Position
game =
  let vAtGround :: Velocity
      vAtGround = fallingVelocity 3 0
      pAtGround :: Position
      pAtGround = 0
      loop :: (Time, Time) -> Position -> Velocity -> MSF m Time Position
      loop (t0, t) p v = fallingPathS (t0, t) p v `MSF.Trans.catchMaybe` loop (t, t + 6) pAtGround (- vAtGround)
   in loop (0, 3) 4.5 0
