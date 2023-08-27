module Main where

import GHC.Float (double2Int)

type V3 = (Float, Float, Float)

negateV3 :: V3 -> V3
addV3 :: V3 -> V3 -> V3
subV3 :: V3 -> V3 -> V3
scaleV3 :: V3 -> Float -> V3

negateV3 (x, y, z) = (-x, -y, -z)
addV3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)
subV3 v1 v2 = v1 `addV3` negateV3 v2
scaleV3 (x, y, z) a = (x * a, y * a, z * a)

type RGB = (Double, Double, Double)

data PPM = PPM
  { size :: (Int, Int),
    content :: [RGB]
  }

data Ray = Ray
  { dir :: V3,
    origin :: V3
  }

rgb2String :: RGB -> String
rgb2String (r, g, b) = f r ++ " " ++ f g ++ " " ++ f b
  where
    f = show . double2Int . (256 *)

encodePPM :: PPM -> String
encodePPM PPM {size = (width, height), content = px} =
  "P3\n"
    ++ show width
    ++ " "
    ++ show height
    ++ "\n255\n"
    ++ foldr (\rgb acc -> rgb2String rgb ++ "\n" ++ acc) "" px

main :: IO ()
main = do
  writeFile "./out.ppm" (encodePPM file)
  where
    file =
      PPM
        { size = (256, 256),
          content =
            [ (j / 256, i / 256, 0) | i <- [0 .. 255], j <- [0 .. 255]
            ]
        }
