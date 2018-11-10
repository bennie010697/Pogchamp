module Pacman where

import Variables
import Model
import Level

import Data.Maybe
import Data.List

newPacman ::Board -> Pacman
newPacman b = Pacman {pacspeed = 0.01 , pacmovement = startPacMove b}

startPacMove :: Board -> Movement
startPacMove b = Movement {hpos = getPacSpawn b, npos = getPacSpawn b}


makePacMove :: LevelData -> LevelData
makePacMove l@(LevelData{lNextMove = dir}) = l {lPacman = huidigePacman{pacmovement = moveUnit huidigePacMove huidigePacSpeed dir}}
                where huidigePacSpeed = pacspeed $ lPacman $ l
                      huidigePacMove  = pacmovement $ lPacman $ l
                      huidigePacman   = lPacman $ l

                
moveUnit :: Movement -> Speed -> Direction -> Movement
moveUnit m s dir | dir == DLeft   = m{hpos = Position{x = (xh - s) , y = yh}}
                 | dir == DRight  = m{hpos = Position{x = (xh + s) , y = yh}}
                 | dir == DUp     = m{hpos = Position{x = xh , y = (yh - s)}}
                 | dir == DDown   = m{hpos = Position{x = xh , y = (yh + s)}}
                 | otherwise      = m
        where xh = x $ hpos $ m
              yh = y $ hpos $ m
