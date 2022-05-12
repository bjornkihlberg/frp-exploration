{-# LANGUAGE BlockArguments #-}

module BouncerSpec (spec) where

import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.MonadicStreamFunction as MSF
import qualified Game.Bouncer as Bouncer
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "game" do
  it "falls at continous rate" do
    let input :: [Bouncer.Time]
        input = [0, 1, 1.5, 3]

        actual :: [Bouncer.Position]
        actual = runIdentity $ MSF.embed Bouncer.game input

        expected :: [Bouncer.Position]
        expected =
          [ 4.5,
            4.5 - 1 / 2,
            4.5 - 1.5 * 1.5 / 2,
            4.5 - 3 * 3 / 2
          ]

    actual `shouldBe` expected

  it "bounces" do
    let input :: [Bouncer.Time]
        input = [0, 1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13, 14, 21]

        actual :: [Bouncer.Position]
        actual = runIdentity $ MSF.embed Bouncer.game input

        expected :: [Bouncer.Position]
        expected =
          [ 4.5,
            4.5 - 1 / 2,
            4.5 - 2 * 2 / 2,
            0,
            4.5 - 2 * 2 / 2,
            4.5 - 1 / 2,
            4.5,
            4.5 - 1 / 2,
            4.5 - 2 * 2 / 2,
            4.5 - 2 * 2 / 2,
            4.5 - 1 / 2,
            4.5,
            4.5 - 1 / 2,
            4.5 - 2 * 2 / 2,
            0
          ]

    actual `shouldBe` expected
