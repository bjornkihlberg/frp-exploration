{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module ParallellSpec (spec) where

import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.MonadicStreamFunction as MSF
import qualified Game.Parallell as Parallell
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "game" do
  it "parallells" do
    let input :: [Parallell.Time]
        input = coerce @[Double] [0, 3, 4, 7, 8, 9]

        actual :: [Double]
        actual = runIdentity $ MSF.embed Parallell.game input

        expected :: [Double]
        expected = [0, 6, 8, 10, 10, 10]
    actual `shouldBe` expected
