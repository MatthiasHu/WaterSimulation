module RndWorld where

import Control.Monad (replicateM)
import Control.Monad.Random
import Data.Array

import IterativeListProcess
import World
import Simulation  -- for soilHarmonization




rndWorld :: (RandomGen g) => (Int, Int) -> Rand g World
rndWorld size = do
--  let w = slope size
  w <- mountainsWorld size
  w' <- randomizeWorld 10.0 w
  return $ (3 `times` soilHarmonization 0.2) $ prepareBoundary w'
  where times n f x | n <= 0     = x
                    | otherwise  = f (times (n-1) f x)


mountainsWorld :: (RandomGen g) => (Int, Int) -> Rand g World
mountainsWorld (w, h) = do
  mountainTops <- replicateM 20 $ oneMountainTop (w, h)
  let height x y = foldl max (-5) $ map (singleHeight x y) mountainTops
      singleHeight x y mT = snd mT - d (x, y) (fst mT)
      d (x1, y1) (x2, y2) = let dx = fromIntegral (x2-x1); dy = fromIntegral (y2-y1)
                            in sqrt(dx*dx+dy*dy)
  return $ array ((0, 0), (w, h))
    [((x, y), WorldElement (height x y) 0 noFlux) | x <- [0..w], y <- [0..h]]

oneMountainTop :: (RandomGen g) => (Int, Int) -> Rand g ((Int, Int), Float)
oneMountainTop (w, h) = do
  x <- getRandomR (-(w `div` 4), w + (w `div` 4))
  y <- getRandomR (-(h `div` 4), h + (h `div` 4))
  height <- getRandomR (10.0, 30.0)
  return ((x, y), height)


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


randomizeWorld :: (RandomGen g) => Float -> World -> Rand g World
randomizeWorld intensity world = do
  newWEs <- mapM (randomizeWorldElement 1.0) (elems world)
  return $ listArray (bounds world) newWEs

randomizeWorldElement :: (RandomGen g) => Float -> WorldElement -> Rand g WorldElement
randomizeWorldElement intensity we = do
  randFloat <- getRandomR (-intensity, intensity)
  return we {soil = soil we + randFloat}


prepareBoundary :: World -> World
prepareBoundary world = world //
  (  [((lx, y), addSoil 5    (world ! (lx, y))) | y <- [ly..hy]]
  ++ [((hx, y), addSoil (-5) (world ! (hx, y))) | y <- [ly..hy]]
  ++ [((x, ly), addSoil 5    (world ! (x, ly))) | x <- [lx..hx]]
  ++ [((x, hy), addSoil 5    (world ! (x, hy))) | x <- [lx..hx]] )
  where ((lx, ly), (hx, hy)) = bounds world
        addSoil s we = we {soil = soil we + s}
