module Ray where

import Vector3
import GHC.Float (double2Int, int2Double, sqrtDouble)

type RGB = V3

data Ray = Ray
  { dir :: V3,
    origin :: V3
  } deriving (Show, Eq)

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

rgb2String :: RGB -> String
rgb2String (r, g, b) = f r ++ " " ++ f g ++ " " ++ f b
  where
    f = show . double2Int . (255.99 *)
