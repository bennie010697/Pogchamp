-- | This module defines how the state changes
--   in response to time and user input
module Movement where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

data Direction = Up
               | Down
               | Left
               | Right

data ObjectMovement = ObjMov Direction Integer

-- upperleft corner is (0,0)
data Position = Pos Integer Integer

getNewPos :: Position -> ObjectMovement -> Position
getNewPos (Pos x y) (ObjMov dir num)
    | dir == Up   = Pos x (y - num)
    | dir == Down = Pos x (y + num)
    | dir == Left = Pos (x - num) y
    | otherwise   = Pos (x + num) y

pacManMove :: Pacman -> ObjectMovement -> Level -> Pacman
pacManMove (Pacman ppos, _, _, _, _, _, _) mov lvl
    | canMoveToPos newPos lvl = newPos -- Pacman can move in requested direction
    | otherwise               = ppos   -- collision with static object; no movement
    where newPos = getNewPos ppos mov

canMoveToPos :: Position -> Level -> Bool
canMoveToPos (Pos x y) (Level s t u f f) = undefined