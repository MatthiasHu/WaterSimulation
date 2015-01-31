module Simulation where

import System.Random
import Data.Array
import Data.List as List (delete)

import IterativeListProcess
import World


simStep :: (RandomGen g) => (g, World) -> (g, World)
simStep = mapSnd waterMovement . rain
  where mapSnd f (a, b) = (a, f b)



rain :: (RandomGen g) => (g, World) -> (g, World)
rain (gen, world) = (gen', array (bounds world) worldList')
  where (gen', worldList') = processIteratively (applyIfNotBoundary raindrop)
                                                gen
                                                (assocs world)
        applyIfNotBoundary f (a, ((x, y), b)) =
          if isBoundary (x, y) then (a, ((x, y), b)) else
            let (a', b') = f (a, b) in (a', ((x, y), b'))
        ((lx, ly), (hx, hy)) = bounds world
        isBoundary (x, y) =  x <= lx || y <= ly || x >= hx || y >= hy


raindrop :: (RandomGen g) => (g, WorldElement) -> (g, WorldElement)
raindrop (gen, we) = (gen', if rand `mod` 1000 <= 1
                            then we {water = water we + dropSize}
                            else we
                     )
  where (rand, gen') = next gen
        dropSize = 0.3


source :: World -> World
source world = world // [(pos, (world ! pos) {water = water (world!pos) +1.0})]
  where pos = (1, 15)


soilHarmonization :: Float -> World -> World
soilHarmonization intensity = processExceptBoundary $ soilHarm_elem intensity

soilHarm_elem :: Float -> World -> (Int, Int) -> WorldElement
soilHarm_elem intensity world pos =
  (world ! pos) {soil = (1-intensity)*oldSoil + intensity/4*neighbourSoils}
  where oldSoil = soil (world ! pos)
        neighbourSoils = sum [soil (world ! neighbour pos dir) | dir <- directions]


waterMovement :: World -> World
waterMovement =  processExceptBoundary calculateFlux_elem
               . processExceptBoundary applyFlux_elem


calculateFlux_elem :: World -> (Int, Int) -> WorldElement
calculateFlux_elem world pos = thisElem {flux = actualFlux}
  where thisElem = world ! pos
        actualFlux = if water thisElem == 0 then noFlux else
          boundFlux $ fn2flux desiredFlux
        desiredFlux dir = max 0 $ (  waterLevel thisElem
                                  - waterLevel (world ! neighbour pos dir)
                                  ) / 5
        boundFlux fl | totalFlux fl <= water thisElem  = fl
                     | otherwise  = (water thisElem / totalFlux fl) `fluxSMult` fl
        waterLevel we = soil we + water we


applyFlux_elem :: World -> (Int, Int) -> WorldElement
applyFlux_elem world pos = thisElem {water = newWater}
  where thisElem = world ! pos
        newWater = max 0 $
          water thisElem
          - totalFlux (flux thisElem)
          + sum [fluxTo (opposite dir) (world ! neighbour pos dir) | dir <- directions]


processExceptBoundary :: (World -> (Int, Int) -> WorldElement) -> World -> World
processExceptBoundary f world =
  array ((lx, ly), (hx, hy))
        [(i, if isBoundary i then world ! i else f world i) | i <- indices]
  where indices = range (bounds world)
        ((lx, ly), (hx, hy)) = bounds world
        isBoundary (x, y) =  x <= lx || y <= ly || x >= hx || y >= hy



fluxAdd :: Flux -> Flux -> Flux
fluxAdd a b = Flux {left  = left a  + left b
                   ,right = right a + right b
                   ,up    = up a    + up b
                   ,down  = down a  + down b
                   }

fluxSMult :: Float -> Flux -> Flux
fluxSMult s fl = Flux {left  = s * left fl
                      ,right = s * right fl
                      ,up    = s * up fl
                      ,down  = s * down fl
                      }
