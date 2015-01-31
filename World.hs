module World where

import Prelude hiding (Left, Right)
import Data.Array.Unboxed


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

fluxTo :: Direction -> WorldElement -> Float
fluxTo Left = left . flux
fluxTo Right = right . flux
fluxTo Up = up . flux
fluxTo Down = down . flux

fn2flux :: (Direction -> Float) -> Flux
fn2flux f = Flux {left = f Left, right = f Right
                 ,up = f Up, down = f Down}

neighbour :: (Int, Int) -> Direction -> (Int, Int)
neighbour (x, y) Left  = (x-1, y)
neighbour (x, y) Right = (x+1, y)
neighbour (x, y) Up    = (x, y+1)
neighbour (x, y) Down  = (x, y-1)

opposite :: Direction -> Direction
opposite Left = Right
opposite Right = Left
opposite Up = Down
opposite Down = Up
