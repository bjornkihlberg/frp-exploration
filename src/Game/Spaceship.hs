{-# LANGUAGE Arrows #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Game.Spaceship where

import qualified Control.Monad.Trans.MSF as MSF.Trans
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity)
import qualified Data.Maybe as Maybe
import Data.MonadicStreamFunction (Arrow ((&&&)), MSF, (<<<), (^<<))
import qualified Data.MonadicStreamFunction as MSF
import qualified Extra
import qualified Extra.Linear as Linear.Extra
import qualified Extra.MSF as MSF.Extra
import Linear ((*^))
import qualified Linear

type Number = Int

newtype Time = Time Number
  deriving (Eq, Ord, Num, Show)

type Game = MSF.Trans.Reader (Time, Precision, Thrust)

type GameSF = MSF Game

newtype Position = Position (Linear.V2 Number)
  deriving (Eq, Ord, Num)

instance Show Position where
  show (Position (Linear.V2 x y)) = show (x, y)

newtype Velocity = Velocity (Linear.V2 Number)
  deriving (Eq, Ord, Num)

instance Show Velocity where
  show (Velocity (Linear.V2 x y)) = show (x, y)

newtype Acceleration = Acceleration (Linear.V2 Number)

newtype IsThrusting = IsThrusting Bool
  deriving (Eq, Show)

newtype Thrust = Thrust Number
  deriving (Num)

newtype Precision = Precision Time
  deriving (Num)

type TargetPosition = Position

timeS :: GameSF a Time
timeS = MSF.constM $ MSF.Trans.asks (\(t, _, _) -> t)

fallingPosition :: Time -> Position -> Velocity -> Acceleration -> Time -> Position
fallingPosition t0 (Position p0) (Velocity v0) (Acceleration a) (subtract t0 -> Time dt) =
  Position $ p0 + (dt *^ v0) `Linear.Extra.v2Div` 1_000 + (dt ^ (2 :: Int) *^ a) `Linear.Extra.v2Div` 2_000_000

-- DONE EXPERIMENT try using ReaderT for global time
fallingPositionS :: Time -> Position -> Velocity -> Acceleration -> GameSF a Position
fallingPositionS t0 p0 v0 a = fallingPosition t0 p0 v0 a ^<< timeS

fallingVelocity :: Time -> Velocity -> Acceleration -> Time -> Velocity
fallingVelocity t0 (Velocity v0) (Acceleration a) (subtract t0 -> Time dt) =
  Velocity $ v0 + (dt *^ a) `Linear.Extra.v2Div` 1_000

-- DONE tests
-- DONE implementation
-- DONE EXPERIMENT try using ReaderT for global time
thrustingS :: forall a. TargetPosition -> Time -> Position -> Velocity -> GameSF a (Position, Velocity)
thrustingS tp =
  let loop :: (Time, Position, Velocity) -> Game (Position, Velocity)
      loop (t0, p0, v0) = do
        (t, Precision dt, Thrust thrust) <- MSF.Trans.ask
        let a = Acceleration $ thrust *^ Linear.Extra.inormalize (coerce $ tp - p0)
            t' = t0 + dt
        if t >= t'
          then loop (t', fallingPosition t0 p0 v0 a t', fallingVelocity t0 v0 a t')
          else pure (fallingPosition t0 p0 v0 a t, fallingVelocity t0 v0 a t)
   in Extra.curry3 (MSF.constM . loop)

-- DONE tests
-- DONE implementation
-- DONE EXPERIMENT try using arrows
-- DONE EXPERIMENT try using ReaderT for global time
chaseS :: forall m. TargetPosition -> Time -> Position -> Velocity -> GameSF (Maybe TargetPosition) (Position, Velocity)
chaseS =
  let loop :: (TargetPosition, Time, Position, Velocity) -> GameSF (Maybe TargetPosition) (Position, Velocity)
      loop (tp0, t0, p0, v0) =
        let msf :: GameSF (Maybe TargetPosition) ((Position, Velocity), Maybe (TargetPosition, Time, Position, Velocity))
            msf = proc mtp -> do
              (p, v) <- thrustingS tp0 t0 p0 v0 -< ()
              t <- timeS -< ()
              MSF.returnA -< ((p, v), (,t,p,v) <$> mtp)
         in MSF.Extra.nswitch msf loop
   in Extra.curry4 loop

-- DONE tests
-- DONE implement
-- DONE cleanup
-- DONE EXPERIMENT try using arrows
-- DONE EXPERIMENT try using ReaderT for global time
movementS :: forall m. TargetPosition -> Time -> Position -> Velocity -> GameSF (Maybe (), Maybe TargetPosition) (Position, Velocity)
movementS =
  let cruisingLoop :: (TargetPosition, Time, Position, Velocity) -> GameSF (Maybe (), Maybe TargetPosition) (Position, Velocity)
      cruisingLoop (tp0, t0, p0, v0) =
        let msf :: GameSF ((Maybe (), Maybe TargetPosition), TargetPosition) (((Position, Velocity), Maybe (TargetPosition, Time, Position, Velocity)), TargetPosition)
            msf = proc ((toggle, mtp), tp) -> do
              p <- fallingPositionS t0 p0 v0 (Acceleration Linear.zero) -< ()
              t <- timeS -< ()
              let tp' = Maybe.fromMaybe tp mtp
              MSF.returnA -< (((p, v0), (tp', t, p, v0) <$ toggle), tp')
         in MSF.Extra.nswitch (MSF.feedback tp0 msf) thrustingLoop

      thrustingLoop :: (TargetPosition, Time, Position, Velocity) -> GameSF (Maybe (), Maybe TargetPosition) (Position, Velocity)
      thrustingLoop (tp0, t0, p0, v0) =
        let msf :: GameSF ((Maybe (), Maybe TargetPosition), TargetPosition) (((Position, Velocity), Maybe (TargetPosition, Time, Position, Velocity)), TargetPosition)
            msf = proc ((toggle, mtp), tp) -> do
              (p, v) <- chaseS tp0 t0 p0 v0 -< mtp
              t <- timeS -< ()
              let tp' = Maybe.fromMaybe tp mtp
              MSF.returnA -< (((p, v), (tp', t, p, v) <$ toggle), tp')
         in MSF.Extra.nswitch (MSF.feedback tp0 msf) cruisingLoop
   in Extra.curry4 cruisingLoop

-- DONE test suite
isThrustingS :: Monad m => MSF m (Maybe ()) IsThrusting
isThrustingS = flip MSF.accumulateWith (IsThrusting False) \case
  Just () -> coerce not
  Nothing -> id

-- DONE implement
-- DONE EXPERIMENT try using ReaderT for global time
game :: MSF Identity (Time, Maybe (), Maybe TargetPosition) (Position, IsThrusting)
game = proc (t, toggle, mtp) -> do
  isThrusting <- isThrustingS -< toggle
  (p, _) <- (MSF.Trans.runReaderS (movementS (Position Linear.zero) 0 (Position Linear.zero) (Velocity Linear.zero))) -< ((t, 10, 1), (toggle, mtp))
  MSF.returnA -< (p, isThrusting)
