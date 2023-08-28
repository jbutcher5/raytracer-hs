module Main where

import GHC.Float (double2Int, int2Double, sqrtDouble)

type V3 = (Double, Double, Double)

data Ray = Ray
  { dir :: V3,
    origin :: V3
  } deriving (Show, Eq)

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

rayAt :: Ray -> Double -> V3
rayAt r a = dir r `scaleV3` a `addV3` origin r

viewportRay :: (Int, Int) -> (Int, Int) -> Ray
viewportRay (x, y) (imageWidth, imageHeight) =
  Ray
    { origin = origin,
      dir = (dx * 0.5 + tlx + dx * int2Double x, -dy * 0.5 + tly - dy * int2Double y, oz + focalLength)
    }
  where
    aspectRatio = 16 / 9
    origin@(ox, oy, oz) = (0, 0, 0) :: V3
    (tlx, tly) = (ox - 0.5 * viewportWidth, oy + 0.5 * viewportHeight) :: (Double, Double)
    (dx, dy) = (viewportWidth / int2Double imageWidth, viewportHeight / int2Double imageHeight) :: (Double, Double)
    focalLength = 1 :: Double
    viewportHeight = 2 :: Double
    viewportWidth = viewportHeight * aspectRatio :: Double

castRay :: Ray -> RGB
castRay r = ((1, 1, 1) `scaleV3` c) `addV3` ((0.5, 0.7, 1.0) `scaleV3` b) --  absV3 . unitV3 $ dir r 
  where
    (_, a, _) = unitV3 $ dir r
    b = 0.5 * (a + 1) :: Double
    c = 1 - b :: Double
      
type RGB = V3

data PPM = PPM
  { size :: (Int, Int),
    content :: [RGB]
  }

rgb2String :: RGB -> String
rgb2String (r, g, b) = f r ++ " " ++ f g ++ " " ++ f b
  where
    f = show . double2Int . (255.99 *)

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
        { size = imageSize,
          content =
            [ castRay $ viewportRay (i, j) imageSize | j <- [0 .. imageHeight - 1], i <- [0 .. imageWidth - 1]
            ]
        }
    imageSize@(imageWidth, imageHeight) = (32, 18):: (Int, Int)
