module Simulation where

import System.Random
import Data.Array
import Data.List as List (delete)

import IterativeListProcess
import World


simStep :: (RandomGen g) => (g, World) -> (g, World)
simStep = mapSnd (soilHarmonization 0.01 . waterMovement) . rain
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
raindrop (gen, we) = (gen', if rand `mod` 1000 <= 10
                            then we {water = newWater
                                    ,flux = (water we / newWater) `fluxSMult` flux we
                                    }
                            else we
                     )
  where (rand, gen') = next gen
        dropSize = 0.3
        newWater = water we + dropSize


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
waterMovement = processExceptBoundary waterMove_elem

waterMove_elem :: World -> (Int, Int) -> WorldElement
waterMove_elem world pos = thisElem {soil = newSoil, water = newWater, flux = fn2flux newFlux}
  where thisElem = world ! pos
        -- water transport:
        newWater = remainingWater + sum [influx dir | dir <- directions]
        remainingWater = water thisElem - sum [drain dir | dir <- directions]
        influx dir = fluxTo (opposite dir) (world ! neighbour pos dir)
                     * water (world ! neighbour pos dir)
        drain dir = fluxTo dir thisElem
                    * water thisElem
        -- soil transport:
        newSoil = soil thisElem + soilTransportFactor * (sum [soilInflux dir | dir <- directions]
                                                        - sum [soilDrain dir | dir <- directions])
        soilInflux dir = influx dir * fluxTo (opposite dir) (world ! neighbour pos dir)
        soilDrain dir = drain dir * fluxTo dir thisElem
        soilTransportFactor = 10.0
        -- water momentum transport:
        newFlux dir = if newWater == 0 then 0 else
          bound025 $ (oldFluxScaled + carriedInFlux) / newWater + gravFlux
          where oldFluxScaled = remainingWater * fluxTo dir thisElem
                carriedInFlux = sum [influx dir2 * fluxTo dir (world ! neighbour pos dir2)
                                    | dir2 <- List.delete dir directions]
                gravFlux = grav * incline
                incline = newSoil + newWater
                          - soil (world ! neighbour pos dir) - water (world ! neighbour pos dir) 
        bound025 x | x < 0     = 0
                   | x > 0.25   = 0.25
                   | otherwise = x
        grav = 0.5



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
