module Vector3 where

import GHC.Float (sqrtDouble)

type V3 = (Double, Double, Double)

negateV3 :: V3 -> V3
addV3 :: V3 -> V3 -> V3
subV3 :: V3 -> V3 -> V3
scaleV3 :: V3 -> Double -> V3
divideV3 :: V3 -> Double -> V3
unitV3 :: V3 -> V3
lengthV3 :: V3 -> Double
absV3 :: V3 -> V3

negateV3 (x, y, z) = (-x, -y, -z)
addV3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
subV3 v1 v2 = v1 `addV3` negateV3 v2
scaleV3 (x, y, z) a = (x * a, y * a, z * a)
divideV3 (x, y, z) a = (x / a, y / a, z / a)
unitV3 v = v `divideV3` lengthV3 v
lengthV3 (x, y, z) = sqrtDouble $ x*x + y*y + z*z
absV3 (x, y, z) = (abs x, abs y, abs z)
