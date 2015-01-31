module RndWorld where

import System.Random
import Data.Array

import IterativeListProcess
import World
import Simulation




rndWorld :: (RandomGen g) => g -> (Int, Int) -> (g, World)
rndWorld g size = mapSnd ((5 `times` soilHarmonization 0.2) . prepareBoundary) $
  randomizeWorld 2.0 g $ slope size
  where mapSnd f (a, b) = (a, f b)
        times n f x | n <= 0     = x
                    | otherwise  = f (times (n-1) f x)


smallWorld :: World
smallWorld = array ((0, 0), (6, 6)) [(i, WorldElement 2 0 noFlux) | i <- range ((0, 0), (6, 6))]
  // [((3, 3), WorldElement 2 1 noFlux)
     ,((4, 3), WorldElement 1 0 noFlux)
     ]

leftRightWorld :: (Int, Int) ->  World
leftRightWorld (w, h) =
  array ((0, 0), (w, h)) [((x, y), WorldElement (if x > (w `div` 2) then 0 else 10)
                                                0
                                                noFlux)
                          | x <- [0..w], y <- [0..h] ]


slope :: (Int, Int) -> World
slope (w, h) =
  array ((0, 0), (w, h)) [((x, y), WorldElement {soil = let nx = fromIntegral (w-x)
                                                            w' = fromIntegral w
                                                        in (nx+nx*nx/w') * 0.3
                                                ,water = 0.0
                                                ,flux = noFlux})
                          | x <- [0..w], y <- [0..h] ]

randomizeWorld :: (RandomGen g) => Float -> g -> World -> (g, World)
randomizeWorld intensity g world = (g', array (bounds world) worldList')
  where (g', worldList') = processIteratively (applyToRightThing $ randomizeWorldElement intensity)
                                              g
                                              (assocs world)
        applyToRightThing f (a, (i, b)) = let (a', b') = f a b in (a', (i, b'))

randomizeWorldElement :: (RandomGen g) => Float -> g -> WorldElement -> (g, WorldElement)
randomizeWorldElement intensity g we = (g', we {soil = soil we + randFloat})
  where (randFloat, g') = randomR (-intensity, intensity) g


prepareBoundary :: World -> World
prepareBoundary world = world //
  (  [((lx, y), addSoil 5    (world ! (lx, y))) | y <- [ly..hy]]
  ++ [((hx, y), addSoil (-5) (world ! (hx, y))) | y <- [ly..hy]]
  ++ [((x, ly), addSoil 5    (world ! (x, ly))) | x <- [lx..hx]]
  ++ [((x, hy), addSoil 5    (world ! (x, hy))) | x <- [lx..hx]] )
  where ((lx, ly), (hx, hy)) = bounds world
        addSoil s we = we {soil = soil we + s}
