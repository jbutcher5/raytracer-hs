module Main where

import GHC.Float (double2Int, int2Double)

type V3 = (Double, Double, Double)

data Ray = Ray
  { dir :: V3,
    origin :: V3
  }

negateV3 :: V3 -> V3
addV3 :: V3 -> V3 -> V3
subV3 :: V3 -> V3 -> V3
scaleV3 :: V3 -> Double -> V3
negateV3 (x, y, z) = (-x, -y, -z)

addV3 (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

subV3 v1 v2 = v1 `addV3` negateV3 v2

scaleV3 (x, y, z) a = (x * a, y * a, z * a)

rayAt :: Ray -> Double -> V3
rayAt r a = dir r `scaleV3` a `addV3` origin r

viewportRay :: V3 -> Double -> (Double, Double) -> (Double, Double) -> (Int, Int) -> Ray
viewportRay origin@(ox, oy, oz) focalLength (tlx, tly) (dx, dy) (x, y) =
  Ray
    { origin = origin,
      dir = (ox + tlx + dx * int2Double x, oy + tly - dy * int2Double y, oz + focalLength)
    }

castRay :: Ray -> RGB
castRay _ = (0, 0, 0)

type RGB = V3

data PPM = PPM
  { size :: (Int, Int),
    content :: [RGB]
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
        { size = imageSize,
          content =
            [ castRay $ viewportRay origin focalLength topLeft pixelDelta (j, i) | i <- [0 .. imageWidth - 1], j <- [0 .. imageHeight - 1]
            ]
        }
    aspectRatio = 16 / 9
    origin@(ox, oy, oz) = (0, 0, 0) :: V3
    imageSize@(imageWidth, imageHeight) = (800, 450) :: (Int, Int)
    topLeft = (ox - 0.5 * viewportWidth, oy + 0.5 * viewportHeight) :: (Double, Double)
    pixelDelta = (viewportWidth / int2Double imageWidth, viewportWidth / int2Double imageHeight) :: (Double, Double)
    focalLength = 1 :: Double
    viewportHeight = 2 :: Double
    viewportWidth = viewportHeight * aspectRatio :: Double
