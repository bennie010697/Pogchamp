module Pacman where

import Variables
import Model
import Level

import Data.Maybe
import Data.List

newPacman ::Board -> Pacman
newPacman b = Pacman {pacspeed = 0.06 , pacmovement = startPacMove b}

startPacMove :: Board -> Movement
startPacMove b = Movement {hpos = getPacSpawn b, npos = getPacSpawn b}

makePacMove :: LevelData -> LevelData
makePacMove l@(LevelData{lNextMove = nextdir, ldirection = dir})        | checkPacInAnyWall (checkPacInWall l {lPacman = huidigePacman{pacmovement = moveUnit huidigePacMove huidigePacSpeed dir}}) = l
                                                                        | otherwise = l {lPacman = huidigePacman{pacmovement = moveUnit huidigePacMove huidigePacSpeed dir}, ltick = not (ltick l)}
                                          where huidigePacSpeed = pacspeed $ huidigePacman
                                                huidigePacMove  = pacmovement $ huidigePacman
                                                huidigePacman   = lPacman $ l

changeldirection :: LevelData -> LevelData
changeldirection l@(LevelData{lNextMove = nextdir, ldirection = dir}) | not (checkPacInAnyWall (checkPacInWall l {lPacman = huidigePacman{pacmovement = moveUnit huidigePacMove huidigePacSpeed nextdir}})) && notReverse l = l{ldirection = nextdir}                                        
                                                                      | otherwise = l
                                                where huidigePacSpeed = pacspeed $ lPacman $ l
                                                      huidigePacMove  = pacmovement $ lPacman $ l
                                                      huidigePacman   = lPacman $ l
notReverse :: LevelData -> Bool
notReverse l@(LevelData{ldirection = dir, lNextMove = nextdir})   | (dir == DLeft) && (nextdir == DRight) = False
                                                                  | (dir == DRight) && (nextdir == DLeft) = False
                                                                  | (dir == DUp)  && (nextdir == DDown)   = False
                                                                  | (dir == DDown) && (nextdir == DUp)    = False
                                                                  | otherwise = True

pacmanDies :: LevelData -> LevelData
pacmanDies l | checkPacInAnyWall (checkInEnemy l) = l{lPacman = huidigePacman{pacmovement = startPacMove board}, llives = lives}
             | otherwise = l
                        where huidigePacman     = lPacman $ l
                              board             = lboard $ l
                              lives             = ((llives $ l) - 1)

nextPacMove :: Movement -> Speed -> Direction -> Movement
nextPacMove m s dir     | dir == DLeft   = m{npos = Position{x = (xh - s) , y = yh}}
                        | dir == DRight  = m{npos = Position{x = (xh + s) , y = yh}}
                        | dir == DUp     = m{npos = Position{x = xh , y = (yh - s)}}
                        | dir == DDown   = m{npos = Position{x = xh , y = (yh + s)}}
                        | otherwise      = m{npos = Position{x = xh , y = yh}}
                  where xh = x $ hpos $ m
                        yh = y $ hpos $ m
                      
moveUnit :: Movement -> Speed -> Direction -> Movement
moveUnit m s dir = m{hpos = (npos $ nextPacMove m s dir), npos = npos $(nextUnit m s dir)}


nextUnit :: Movement -> Speed -> Direction -> Movement
nextUnit m s dir = m{hpos = (hpos $ m) ,npos = (npos $ nextPacMove (nextPacMove m s dir) s dir)}




boxCollideWalls :: Pacman -> (Int, Int) -> Bool
boxCollideWalls p (x,y) = fromIntegral x <= right && fromIntegral x >= left && fromIntegral y <= top && fromIntegral y >= bottom
                  where left              =  a - 0.98
                        right             =  a + 0.98
                        top               =  b + 0.98
                        bottom            =  b - 0.98
                        (Position a b)    =  npos $ pacmovement p 
                        
boxCollideEnemy :: Pacman -> (Int, Int) -> Bool
boxCollideEnemy p (x,y) = fromIntegral x <= right && fromIntegral x >= left && fromIntegral y <= top && fromIntegral y >= bottom
            where left              =  a - 0.5
                  right             =  a + 0.5
                  top               =  b + 0.5
                  bottom            =  b - 0.5
                  (Position a b)    =  hpos $ pacmovement p

checkInEnemy :: LevelData -> [Bool]
checkInEnemy l@(LevelData{lPacman = pacman,lenemylist = list}) = map (boxCollideEnemy pacman) lijst
                  where lijst = map (posTuple . hpos . emovement) list

                                 
posTuple :: Position -> (Int, Int)
posTuple (Position a b) = (round a , round b)


boxCollide :: Pacman -> (Int, Int) -> Bool
boxCollide p (x,y) = fromIntegral x <= right && fromIntegral x >= left && fromIntegral y <= top && fromIntegral y >= bottom
            where left              =  a - 0.5
                  right             =  a + 0.5
                  top               =  b + 0.5
                  bottom            =  b - 0.5
                  (Position a b)    =  hpos $ pacmovement p

checkPacInWall :: LevelData -> [Bool]
checkPacInWall l@(LevelData{lPacman = pacman,lwalllist = list}) = map (boxCollideWalls pacman) list

checkPacInAnyWall :: [Bool] -> Bool
checkPacInAnyWall []  = False
checkPacInAnyWall (b:bs) | b = True
                         | otherwise = checkPacInAnyWall bs

updateFruits :: LevelData -> LevelData
updateFruits l@(LevelData{lPacman = pacman, lfruitlist = list, lScore = huidigescore}) = l{lfruitlist = filter (not . boxCollide pacman) $ list, lScore = scorechanger (map (boxCollide pacman) list) huidigescore}

scorechanger :: [Bool] -> Int -> Int      
scorechanger [] i = i
scorechanger (b:bs) i | b         = i + 10 
                      | otherwise = scorechanger bs i

