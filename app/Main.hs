module Main where

import Control.Parallel.Strategies (parMap, rpar)
import File
import Ray
import Vector3

main :: IO ()
main = do
  writeFile "./out.ppm" (encodePPM file)
  where
    file =
      PPM
        { size = imageSize,
          content =
            parMap
              rpar
              (castRay . viewportRay imageSize)
              [ (x, y) | y <- [0 .. imageHeight - 1], x <- [0 .. imageWidth - 1]
              ]
        }
    imageSize@(imageWidth, imageHeight) = (19200, 10800) :: (Int, Int)
