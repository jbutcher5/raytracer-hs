module Vector3 where

import GHC.Float (sqrtDouble, double2Int)

type V3 = (Double, Double, Double)
type RGB = V3

negateV3 :: V3 -> V3
addV3 :: V3 -> V3 -> V3
subV3 :: V3 -> V3 -> V3
scaleV3 :: V3 -> Double -> V3
divideV3 :: V3 -> Double -> V3
unitV3 :: V3 -> V3
lengthV3 :: V3 -> Double
absV3 :: V3 -> V3
dot :: V3 -> V3 -> Double 

negateV3 (x, y, z) = (-x, -y, -z)
addV3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
subV3 v1 v2 = v1 `addV3` negateV3 v2
scaleV3 (x, y, z) a = (x * a, y * a, z * a)
divideV3 (x, y, z) a = (x / a, y / a, z / a)
unitV3 v = v `divideV3` lengthV3 v
lengthV3 (x, y, z) = sqrtDouble $ x*x + y*y + z*z
absV3 (x, y, z) = (abs x, abs y, abs z)
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

background :: V3 -> RGB
background v = ((1, 1, 1) `scaleV3` c) `addV3` ((0.5, 0.7, 1.0) `scaleV3` b) 
  where
    (_, a, _) = unitV3 v 
    b = 0.5 * (a + 1) :: Double
    c = 1 - b :: Double

rgb2String :: RGB -> String
rgb2String (r, g, b) = f r ++ " " ++ f g ++ " " ++ f b
  where
    f = show . double2Int . (255.99 *)

 
