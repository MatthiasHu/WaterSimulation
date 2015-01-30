module Simulation where

import System.Random
import Data.Array

import World


simStep :: (StdGen, World) -> (StdGen, World)
simStep (gen, world) = (gen', array (bounds world) worldList')
  where (gen', worldList') = processIteratively (applyToRightThing raindrop)
                                              gen
                                              (assocs world)
        applyToRightThing f (a, (b, c)) = let (a', c') = f (a, c) in (a', (b, c'))


processIteratively :: ((a, b) -> (a, b)) -> a -> [b] -> (a, [b])
processIteratively _ a [] = (a, [])
processIteratively f a (b:bs) = (a'', b':bs')
                              where (a', b') = f (a, b)
                                    (a'', bs') = processIteratively f a' bs


raindrop :: (StdGen, WorldElement) -> (StdGen, WorldElement)
raindrop (gen, we) = let (rand, gen') = next gen in
                     (gen', if rand `mod` 100 == 0
                               then we {water = water we + 0.1}
                               else we )
