{-# LANGUAGE BlockArguments #-}

module CounterSpec (spec) where

import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.MonadicStreamFunction as MSF
import qualified Game.Counter as Counter
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "game" do
  it "increments and decrements on events" do
    let input :: [Maybe Counter.Message]
        input =
          [ Nothing,
            Nothing,
            Just Counter.Increment,
            Nothing,
            Nothing,
            Just Counter.Decrement,
            Just Counter.Increment,
            Nothing,
            Just Counter.Decrement,
            Just Counter.Decrement,
            Just Counter.Increment,
            Just Counter.Increment,
            Nothing,
            Just Counter.Decrement,
            Nothing,
            Nothing
          ]

        actual :: [Int]
        actual = runIdentity $ MSF.embed Counter.game input

        expected :: [Int]
        expected = [0, 0, 1, 1, 1, 0, 1, 1, 0, -1, 0, 1, 1, 0, 0, 0]

    actual `shouldBe` expected
