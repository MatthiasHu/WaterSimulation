module RndWorld where

import System.Random
import Data.Array

import World



rndWorld :: (RandomGen g) => g -> (Int, Int) -> World
rndWorld g (w, h) = array ((0, 0), (w, h))
  [((x, y), WorldElement {soil = fromIntegral (w-x) * 0.2
                         ,water = 0
                         ,flux = noFlux})
    | x <- [0..w], y <- [0..h] ]


noFlux :: Flux
noFlux = Flux 0 0 0 0
