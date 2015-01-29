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
