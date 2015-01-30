module World where

import Prelude hiding (Left, Right)
import Data.Array


type World = Array (Int, Int) WorldElement


data WorldElement = WorldElement
  {soil :: Float
  ,water :: Float
  ,flux :: Flux
  }

data Flux = Flux
  {left, right, up, down :: Float}



data Direction = Left | Right | Up | Down
  deriving (Eq, Ord, Show, Read)

directions :: [Direction]
directions = [Left, Right, Up, Down]

fluxTo :: Direction -> Flux -> Float
fluxTo Left = left
fluxTo Right = right
fluxTo Up = up
fluxTo Down = down


neighbour :: (Int, Int) -> Direction -> (Int, Int)
neighbour (x, y) Left  = (x-1, y)
neighbour (x, y) Right = (x+1, y)
neighbour (x, y) Up    = (x, y+1)
neighbour (x, y) Down  = (x, y-1)
