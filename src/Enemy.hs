module Enemy where

import Variables
import Model
import Level

import Data.Maybe
import Data.List

newEnemy :: Board -> EnemyType -> Enemy
newEnemy b t = Enemy { etype = t, estate = Hunts, espeed = 0.04, emovement = startEnemyMove b }

startEnemyMove :: Board -> Movement
startEnemyMove b = Movement { hpos = getEnemySpawn b, npos = getEnemySpawn b }

-- dit moet nog worden aangepast
makeEnemyMove :: LevelData -> LevelData
makeEnemyMove l@(LevelData{lNextMove = dir}) = l {lPacman = huidigePacman{pacmovement = moveUnit huidigePacMove huidigePacSpeed dir}}
                where hspeed = espeed $ lPacman $ l
                      hmove  = emovement $ lPacman $ l
                      huidigePacman   = lPacman $ l
--

moveUnit :: Movement -> Speed -> Direction -> Movement
moveUnit m s dir | dir == DLeft  = m{hpos = Position{x = (xh - s) , y = yh}}
                 | dir == DRight = m{hpos = Position{x = (xh + s) , y = yh}}
                 | dir == DUp    = m{hpos = Position{x = xh , y = (yh - s)}}
                 | dir == DDown  = m{hpos = Position{x = xh , y = (yh + s)}}
                 | otherwise     = m
        where xh = x $ hpos $ m
              yh = y $ hpos $ m
