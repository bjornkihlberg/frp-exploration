{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module SpaceshipSpec (spec) where

import qualified Control.Monad as Control
import qualified Control.Monad.Trans.MSF as MSF.Trans
import qualified Data.Bifunctor as Bifunctor
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (runIdentity))
import Data.MonadicStreamFunction (Arrow ((&&&)), MSF, (<<<))
import qualified Data.MonadicStreamFunction as MSF
import qualified Extra.Linear as Linear.Extra
import qualified GHC.Float as Float
import qualified GHC.Stack as Stack
import qualified Game.Spaceship as Spaceship
import Linear ((*^), (^*))
import qualified Linear
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Test.Hspec as Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.Gen (Gen)
import qualified Test.QuickCheck.Gen as Gen

spec :: Spec
spec = do
  describe "isThrustingS" do
    it "toggles thruster" do
      let input :: [Maybe ()]
          input = coerce [Nothing, Nothing, Nothing, Just (), Nothing, Nothing, Nothing, Just (), Nothing, Nothing]

          actual :: [Spaceship.IsThrusting]
          actual = runIdentity $ MSF.embed Spaceship.isThrustingS input

          expected :: [Spaceship.IsThrusting]
          expected = coerce [False, False, False, True, True, True, True, False, False, False]

      actual `shouldBe` expected

  describe "thrustingS" do
    prop "thrusts" do
      thrust <- Spaceship.Thrust <$> QuickCheck.arbitrary
      t0 <- QuickCheck.arbitrary
      let precision :: Spaceship.Precision = 500

      let t1 = 1_000 + t0; t2 = 1_200 + t0; t3 = 1_300 + t0; t4 = 1_750 + t0; t5 = 3_000 + t0

      tp <- uncurry Linear.V2 <$> QuickCheck.arbitrary @(Int, Int)
      p0 <- Spaceship.Position . uncurry Linear.V2 <$> QuickCheck.arbitrary
      v0 <- Spaceship.Velocity . uncurry Linear.V2 <$> QuickCheck.arbitrary

      let f dt (Spaceship.Position p, Spaceship.Velocity v) =
            let a = coerce thrust *^ Linear.Extra.inormalize (tp - p)
             in ( Spaceship.Position (p + ((v ^* dt) `Linear.Extra.v2Div` 1_000) + (a ^* (dt ^ 2)) `Linear.Extra.v2Div` 2_000_000)
                , Spaceship.Velocity (v + ((a ^* dt) `Linear.Extra.v2Div` 1_000))
                )

      let input :: [(Spaceship.Time, Spaceship.Precision, Spaceship.Thrust)]
          input = (,precision,thrust) <$> coerce @[Spaceship.Number] [t0, t1, t2, t3, t4, t5]

          actual :: [(Spaceship.Position, Spaceship.Velocity)]
          actual = runIdentity $ MSF.embed (MSF.Trans.runReaderS (Spaceship.thrustingS (coerce tp) (Spaceship.Time t0) p0 v0)) ((,()) <$> input)

          expected :: [(Spaceship.Position, Spaceship.Velocity)]
          expected =
            [ (p0, v0)
            , f 500 $ f 500 (p0, v0)
            , f 200 $ f 500 $ f 500 (p0, v0)
            , f 300 $ f 500 $ f 500 (p0, v0)
            , f 250 $ f 500 $ f 500 $ f 500 (p0, v0)
            , f 500 $ f 500 $ f 500 $ f 500 $ f 500 $ f 500 (p0, v0)
            ]

      pure (actual `shouldBe` expected)

  describe "chaseS" do
    prop "chases" do
      thrust <- Spaceship.Thrust <$> QuickCheck.arbitrary
      t0 <- QuickCheck.arbitrary
      let precision :: Spaceship.Precision = 500

      let t1 = 1_000 + t0; t2 = 1_200 + t0; t3 = 1_300 + t0; t4 = 1_750 + t0; t5 = 3_000 + t0

      tp0 :: Linear.V2 Spaceship.Number <- uncurry Linear.V2 <$> QuickCheck.arbitrary
      tp1 :: Linear.V2 Spaceship.Number <- (uncurry Linear.V2 <$> QuickCheck.arbitrary) `Gen.suchThat` (/= tp0)
      tp2 :: Linear.V2 Spaceship.Number <- (uncurry Linear.V2 <$> QuickCheck.arbitrary) `Gen.suchThat` (/= tp1)
      tp5 :: Linear.V2 Spaceship.Number <- (uncurry Linear.V2 <$> QuickCheck.arbitrary) `Gen.suchThat` (/= tp2)

      p0 <- Spaceship.Position . uncurry Linear.V2 <$> QuickCheck.arbitrary
      v0 <- Spaceship.Velocity . uncurry Linear.V2 <$> QuickCheck.arbitrary

      let f tp dt (Spaceship.Position p, Spaceship.Velocity v) =
            let a = coerce thrust *^ Linear.Extra.inormalize (tp - p)
             in ( Spaceship.Position (p + ((v ^* dt) `Linear.Extra.v2Div` 1_000) + (a ^* (dt ^ 2)) `Linear.Extra.v2Div` 2_000_000)
                , Spaceship.Velocity (v + ((a ^* dt) `Linear.Extra.v2Div` 1_000))
                )

          input :: [((Spaceship.Time, Spaceship.Precision, Spaceship.Thrust), Maybe Spaceship.Position)]
          input =
            (\(t, mtp) -> ((t, precision, thrust), mtp))
              <$> coerce @[(Spaceship.Number, Maybe (Linear.V2 Spaceship.Number))]
                [ (t0, Nothing)
                , (t1, Just tp1)
                , (t2, Just tp2)
                , (t3, Nothing)
                , (t4, Nothing)
                , (t5, Just tp5)
                ]

          actual :: [(Spaceship.Position, Spaceship.Velocity)]
          actual = runIdentity $ MSF.embed (MSF.Trans.runReaderS (Spaceship.chaseS (coerce tp0) (Spaceship.Time t0) p0 v0)) input

          expected :: [(Spaceship.Position, Spaceship.Velocity)]
          expected =
            [ (p0, v0)
            , f tp0 500 $ f tp0 500 (p0, v0)
            , f tp1 200 $ f tp0 500 $ f tp0 500 (p0, v0)
            , f tp2 100 $ f tp1 200 $ f tp0 500 $ f tp0 500 (p0, v0)
            , f tp2 50 $ f tp2 500 $ f tp1 200 $ f tp0 500 $ f tp0 500 (p0, v0)
            , f tp2 300 $ f tp2 500 $ f tp2 500 $ f tp2 500 $ f tp1 200 $ f tp0 500 $ f tp0 500 (p0, v0)
            ]

      pure (actual `shouldBe` expected)

  describe "movementS" do
    prop "movements" do
      thrust <- Spaceship.Thrust <$> QuickCheck.arbitrary
      t0 <- QuickCheck.arbitrary
      let precision :: Spaceship.Precision = 500

      tp0 :: Linear.V2 Spaceship.Number <- uncurry Linear.V2 <$> QuickCheck.arbitrary
      tp1 :: Linear.V2 Spaceship.Number <- (uncurry Linear.V2 <$> QuickCheck.arbitrary) `Gen.suchThat` (/= tp0)
      tp2 :: Linear.V2 Spaceship.Number <- (uncurry Linear.V2 <$> QuickCheck.arbitrary) `Gen.suchThat` (/= tp1)
      tp5 :: Linear.V2 Spaceship.Number <- (uncurry Linear.V2 <$> QuickCheck.arbitrary) `Gen.suchThat` (/= tp2)
      tp6 :: Linear.V2 Spaceship.Number <- (uncurry Linear.V2 <$> QuickCheck.arbitrary) `Gen.suchThat` (/= tp5)
      tp7 :: Linear.V2 Spaceship.Number <- (uncurry Linear.V2 <$> QuickCheck.arbitrary) `Gen.suchThat` (/= tp6)

      p0 <- Spaceship.Position . uncurry Linear.V2 <$> QuickCheck.arbitrary
      v0 <- Spaceship.Velocity . uncurry Linear.V2 <$> QuickCheck.arbitrary

      let f tp dt (Spaceship.Position p, Spaceship.Velocity v) =
            let a = coerce thrust *^ Linear.Extra.inormalize (tp - p)
             in ( Spaceship.Position (p + ((v ^* dt) `Linear.Extra.v2Div` 1_000) + (a ^* (dt ^ 2)) `Linear.Extra.v2Div` 2_000_000)
                , Spaceship.Velocity (v + ((a ^* dt) `Linear.Extra.v2Div` 1_000))
                )

          g dt (Spaceship.Position p, Spaceship.Velocity v) =
            ( Spaceship.Position (p + ((v ^* dt) `Linear.Extra.v2Div` 1_000))
            , Spaceship.Velocity v
            )

          input :: [((Spaceship.Time, Spaceship.Precision, Spaceship.Thrust), (Maybe (), Maybe Spaceship.Position))]
          input =
            (\(t, x) -> ((t, precision, thrust), x))
              <$> coerce @[(Spaceship.Number, (Maybe (), Maybe (Linear.V2 Spaceship.Number)))]
                [ (t0, (Nothing, Nothing))
                , (1_000 + t0, (Just (), Just tp1))
                , (1_200 + t0, (Nothing, Just tp2))
                , (1_300 + t0, (Nothing, Nothing))
                , (1_750 + t0, (Just (), Nothing))
                , (3_000 + t0, (Nothing, Just tp5))
                , (3_100 + t0, (Nothing, Just tp6))
                , (3_500 + t0, (Just (), Just tp7))
                , (4_200 + t0, (Nothing, Nothing))
                ]

          actual :: [(Spaceship.Position, Spaceship.Velocity)]
          actual = runIdentity $ MSF.embed (MSF.Trans.runReaderS (Spaceship.movementS (coerce tp0) (Spaceship.Time t0) p0 v0)) input

          expected :: [(Spaceship.Position, Spaceship.Velocity)]
          expected =
            [ (p0, v0)
            , g 1_000 (p0, v0)
            , f tp1 200 $ g 1_000 (p0, v0)
            , f tp2 100 $ f tp1 200 $ g 1_000 (p0, v0)
            , f tp2 50 $ f tp2 500 $ f tp1 200 $ g 1_000 (p0, v0)
            , g 1_250 $ f tp2 50 $ f tp2 500 $ f tp1 200 $ g 1_000 (p0, v0)
            , g 1_350 $ f tp2 50 $ f tp2 500 $ f tp1 200 $ g 1_000 (p0, v0)
            , g 1_750 $ f tp2 50 $ f tp2 500 $ f tp1 200 $ g 1_000 (p0, v0)
            , f tp7 200 $ f tp7 500 $ g 1_750 $ f tp2 50 $ f tp2 500 $ f tp1 200 $ g 1_000 (p0, v0)
            ]

      pure (actual `shouldBe` expected)
