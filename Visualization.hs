module Visualization where

import Data.Array
import Graphics.Gloss

import World


visualize :: World -> Picture
visualize world = Pictures
  [let e = world ! (x, y) in
   Color (soilColor (soil e)) $ coordinateSquare x y
   | (x, y) <- indices world]
  where
    coordinateSquare x y = Polygon [(l x, l y), (h x, l y), (h x, h y), (l x, h y)]
    l n = fromIntegral n     * scale
    h n = fromIntegral (n+1) * scale
    scale = 10


soilColor :: Float -> Color
soilColor s = makeColor (bound $  0.1+s*0.07)
                        (bound $  0.2-s*0.00)
                        (bound $  0.0+s*0.02)
                        1
  where bound x | x < 0     = 0
                | x > 1     = 1
                | otherwise = x

bgcolor :: Color
bgcolor = makeColor 0 0 0 1
