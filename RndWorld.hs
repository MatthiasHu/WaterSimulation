module RndWorld where

import System.Random
import Data.Array

import IterativeListProcess
import World




rndWorld :: (RandomGen g) => g -> (Int, Int) -> (g, World)
rndWorld g size = randomizeWorld g $ inclinedPlane size


inclinedPlane :: (Int, Int) -> World
inclinedPlane (w, h) =
  array ((0, 0), (w, h)) [((x, y), WorldElement {soil = fromIntegral (w-x) * 0.3
                                                ,water = 0.0
                                                ,flux = noFlux})
                          | x <- [0..w], y <- [0..h] ]

randomizeWorld :: (RandomGen g) => g -> World -> (g, World)
randomizeWorld g world = (g', array (bounds world) worldList')
  where (g', worldList') = processIteratively (applyToRightThing randomizeWorldElement)
                                              g
                                              (assocs world)
        applyToRightThing f (a, (i, b)) = let (a', b') = f a b in (a', (i, b'))

randomizeWorldElement :: (RandomGen g) => g -> WorldElement -> (g, WorldElement)
randomizeWorldElement g we = (g', we {soil = soil we + randFloat})
  where (randFloat, g') = randomR (-1.0, 1.0) g


noFlux :: Flux
noFlux = Flux 0 0 0 0
