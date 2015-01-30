module Simulation where

import System.Random
import Data.Array

import IterativeListProcess
import World


simStep :: (RandomGen g) => (g, World) -> (g, World)
simStep = mapSnd soilHarmonization
  where mapSnd f (a, b) = (a, f b)



rain :: (RandomGen g) => (g, World) -> (g, World)
rain (gen, world) = (gen', array (bounds world) worldList')
  where (gen', worldList') = processIteratively (applyToRightThing raindrop)
                                                gen
                                                (assocs world)
        applyToRightThing f (a, (b, c)) = let (a', c') = f (a, c) in (a', (b, c'))


raindrop :: (RandomGen g) => (g, WorldElement) -> (g, WorldElement)
raindrop (gen, we) = let (rand, gen') = next gen in
                     (gen', if rand `mod` 100 == 0
                               then we {water = water we + 0.1}
                               else we )



soilHarmonization :: World -> World
soilHarmonization = processExceptBoundary soilHarm_elem

soilHarm_elem :: World -> (Int, Int) -> WorldElement
soilHarm_elem world pos = (world ! pos) {soil = (1-intensity)*oldSoil + intensity/4*neighbourSoils}
  where oldSoil = soil (world ! pos)
        neighbourSoils = sum [soil (world ! neighbour pos dir) | dir <- directions]
        intensity = 0.005


processExceptBoundary :: (World -> (Int, Int) -> WorldElement) -> World -> World
processExceptBoundary f world =
  array ((lx, ly), (hx, hy))
        [(i, if isBoundary i then world ! i else f world i) | i <- indices]
  where indices = range (bounds world)
        ((lx, ly), (hx, hy)) = bounds world
        isBoundary (x, y) =  x <= lx || y <= ly || x >= hx || y >= hy
