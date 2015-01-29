
import Prelude hiding (Left, Right)
import System.Random
import Graphics.Gloss

import World
import RndWorld
import Visualization


main = do
  gen <- getStdGen
  let pic = visualize (rndWorld gen (20, 20))
  display (InWindow "WaterSimulation" (512, 512) (0, 0))
          bgcolor
          pic
