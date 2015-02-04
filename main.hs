
import Prelude hiding (Left, Right)
import System.Random
import Control.Monad.Random
import Graphics.Gloss

import World
import RndWorld
import Visualization
import Simulation


main = do
  gen0 <- getStdGen
  let (world0, gen1) = runRand (rndWorld (50, 50)) gen0
  simulate (InWindow "WaterSimulation" (512, 512) (0, 0))
           bgcolor
           4   -- fps
           (gen1, world0)
           (\(_, world) -> Translate (-240) (-240) (visualize world))
           (const $ const simStep)
