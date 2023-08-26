module Main where

import GHC.Float (double2Int)

type RGB = (Double, Double, Double)

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
        { size = (256, 256),
          content =
            [ (j / 256, i / 256, 0) | i <- [0 .. 255], j <- [0 .. 255]
            ]
        }
