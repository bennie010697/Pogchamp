-- | This module defines how the state changes
--   in response to time and user input
module Movement where

import Model
import Characters

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

data Direction = Up
               | Down
               | Left
               | Right

data ObjectMovement = ObjMov Direction Integer


getNewPos :: Position -> ObjectMovement -> Position
getNewPos (Pos x y) (ObjMov dir num)
    | dir == Up    && (y - num) > 0 = Pos x (y - num)
    | dir == Down  && (y + num) < 1 {- level.height -} = Pos x (y + num)
    | dir == Left  && (x - num) > 0 = Pos (x - num) y
    | dir == Right && (x + num) < 1 {- level.width -}  = Pos (x + num) y
    | otherwise   = Pos x y

pacManMove :: Pacman -> ObjectMovement -> Board -> Pacman
pacManMove (Pacman ppos, _, _, _, _, _, _) mov board
    | canMoveToPos newPos board = newPos -- Pacman can move in requested direction
    | otherwise                 = ppos   -- collision with static object; no movement
    where newPos = getNewPos ppos mov

canMoveToPos :: Position -> Board -> Bool
canMoveToPos (Pos x y) board = undefined