module File where

import Vector3

data PPM = PPM
  { size :: (Int, Int),
    content :: [RGB]
  }

encodePPM :: PPM -> String
encodePPM PPM {size = (width, height), content = px} =
  "P3\n"
    ++ show width
    ++ " "
    ++ show height
    ++ "\n255\n"
    ++ foldr (\rgb acc -> rgb2String rgb ++ "\n" ++ acc) "" px
