
import Prelude hiding (Left, Right)
import System.Random
import Graphics.Gloss

import World
import RndWorld
import Visualization
import Simulation


main = do
  gen0 <- getStdGen
  let (gen1, world0) = rndWorld gen0 (50, 40)
  simulate (InWindow "WaterSimulation" (512, 512) (0, 0))
           bgcolor
           5  -- fps
           (gen1, world0)
           (\(_, world) -> Translate (-240) (-240) (visualize world))
           (const $ const simStep)
