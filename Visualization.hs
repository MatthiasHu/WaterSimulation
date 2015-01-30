module Visualization where

import Data.Array
import Graphics.Gloss

import World


visualize :: World -> Picture
visualize world = Pictures
  [let e = world ! (x, y) in
   Pictures [Color (soilColor (soil e)) $ coordinateSquare x y
            ,Color (waterColor (water e)) $ coordinateSquare x y]
   | (x, y) <- indices world]
  where
    coordinateSquare x y = Polygon [(l x, l y), (h x, l y), (h x, h y), (l x, h y)]
    l n = fromIntegral n     * scale
    h n = fromIntegral (n+1) * scale
    scale = 10


soilColor :: Float -> Color
soilColor s = makeColor (bound $  0.0+s*0.08)
                        (bound $  0.2+s*0.03)
                        (bound $  0.0+s*0.00)
                        1

waterColor :: Float -> Color
waterColor w = makeColor 0 0 1 (bound w)


bound :: Float -> Float
bound x | x < 0     = 0
        | x > 1     = 1
        | otherwise = x


bgcolor :: Color
bgcolor = makeColor 0 0 0 1
