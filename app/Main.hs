module Main where

import Vector3
import Ray
import File

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
