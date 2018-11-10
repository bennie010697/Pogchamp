module Pacman where

import Variables
import Model
import Level

import Data.Maybe
import Data.List

newPacman ::Board -> Pacman
newPacman b = Pacman {pacspeed = 0.1 , pacmovement = startPacMove b}

startPacMove :: Board -> Movement
startPacMove b = Movement {hpos = getPacSpawn b, npos = getPacSpawn b}




{-makePacMove :: Leveldata -> Leveldata
makePacMove leveldata{lNextMove = dir} | dir == DLeft   = leveldata {lPacman{pacmovement =}}
                                       | dir == DRight  = leveldata {lPacman{pacmovement =}}
                                       | dir == DUp     = leveldata {lPacman{pacmovement =}}
                                       | dir == DDown   = leveldata {lPacman{pacmovement =}}
-}


