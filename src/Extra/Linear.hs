module Extra.Linear where

import qualified GHC.Float as Float
import qualified Linear

v2Div :: Integral a => Linear.V2 a -> a -> Linear.V2 a
v2Div (Linear.V2 x y) z = Linear.V2 (x `div` z) (y `div` z)

inormalize :: Linear.V2 Int -> Linear.V2 Int
inormalize = fmap Float.double2Int . Linear.normalize . fmap Float.int2Double
