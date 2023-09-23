module Ray where

import Vector3
import GHC.Float (double2Int, int2Double, sqrtDouble)

data Ray = Ray
  { dir :: V3,
    origin :: V3
  } deriving (Show, Eq)

rayAt :: Ray -> Double -> V3
rayAt r a = dir r `scaleV3` a `addV3` origin r

viewportRay :: (Int, Int) -> (Int, Int) -> Ray
viewportRay (imageWidth, imageHeight) (x, y) =
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
castRay r = if hitSphere (0, 0, 1) 0.5 r then (1, 0, 0) else bg 
  where
    bg = background $ dir r 

hitSphere :: V3 -> Double -> Ray -> Bool
hitSphere centre radius Ray { dir = d, origin = o } = discriminant >= 0
  where
    oc = o `subV3` centre
    a = dot d d
    b = 2 * dot oc d 
    c = dot oc oc - radius*radius
    discriminant = b*b - 4*a*c
