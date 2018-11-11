module Pacman where

import Variables
import Model
import Level

import Data.Maybe
import Data.List

newPacman ::Board -> Pacman
newPacman b = Pacman {pacspeed = 0.04 , pacmovement = startPacMove b}

startPacMove :: Board -> Movement
startPacMove b = Movement {hpos = getPacSpawn b, npos = getPacSpawn b}

makePacMove :: LevelData -> LevelData
makePacMove l@(LevelData{lNextMove = dir}) = l {lPacman = huidigePacman{pacmovement = moveUnit huidigePacMove huidigePacSpeed dir}, ltick = not (ltick l)}
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


boxCollide :: Pacman -> (Int, Int) -> Bool
boxCollide p (x,y) = fromIntegral x <= right && fromIntegral x >= left && fromIntegral y <= top && fromIntegral y >= bottom
            where left              =  a - 0.5
                  right             =  a + 0.5
                  top               =  b + 0.5
                  bottom            =  b - 0.5
                  (Position a b)    =  hpos $ pacmovement p

updateFruits :: LevelData -> LevelData
updateFruits l@(LevelData{lPacman = pacman, lfruitlist = list, lScore = huidigescore}) = l{lfruitlist = filter (not . boxCollide pacman) $ list, lScore = scorechanger (map (boxCollide pacman) list) huidigescore}

scorechanger :: [Bool] -> Int -> Int      
scorechanger [] i = i
scorechanger (b:bs) i | b         = i + 10 
                      | otherwise = scorechanger bs i

