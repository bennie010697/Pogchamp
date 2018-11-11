module Enemy where

import Variables
import Model
import Level

import Data.Maybe
import Data.List
import System.Random

newEnemy :: Board -> EnemyState -> Int -> Enemy
newEnemy b t i = Enemy {estate = t, espeed = 0.04, emovement = startEnemyMove b i , edir = DUp, enextdir = DUp}

startEnemyMove :: Board -> Int -> Movement
startEnemyMove b i = Movement { hpos = getEnemySpawn b i, npos = getEnemySpawn b i}

moveEnemies :: LevelData -> LevelData
moveEnemies l = l{lenemylist = makeEnemyMove l e, lgen = snd (randomGenerator huidigelgen)}
            where e = lenemylist $ l
                  huidigelgen = lgen $ l

makeEnemyMove :: LevelData -> [Enemy] -> [Enemy]
makeEnemyMove l e = map (moveOneEnemy l) e


--checkEnemyInAnyWall (checkEnemyInWall e{emovement = (enextUnit m s dir)} l)

moveOneEnemy :: LevelData -> Enemy -> Enemy
moveOneEnemy l e@(Enemy{espeed = s, emovement = m, edir = dir, enextdir = nextdir}) | checkCross e l || checkEnemyInAnyWall (checkEnemyInWall e{emovement = (enextUnit m s dir)} l) = doEnemyMoveMentGedrag e l 
                                                                                    | otherwise = e{emovement = emoveUnit m s dir}


doEnemyMoveMentGedrag :: Enemy -> LevelData -> Enemy
doEnemyMoveMentGedrag e@(Enemy{estate = status}) l | status == Hunts = huntgedrag e l
                                                   | status == MovesRandom = moverandom e l
                                                   | otherwise = e

moverandom :: Enemy -> LevelData -> Enemy
moverandom e@(Enemy{emovement = m, edir = curdir, espeed = s}) l           | fst (randomGenerator (lgen l)) < 25  && moveIsPossible e{edir = DLeft} l      && not (curdir == DRight)   = e{edir = DLeft, emovement = emoveUnit m s DLeft}
                                                                           | fst (randomGenerator (lgen l)) < 50  && moveIsPossible e{edir = DRight} l     && not (curdir == DLeft)    = e{edir = DRight, emovement = emoveUnit m s DRight}
                                                                           | fst (randomGenerator (lgen l)) < 75  && moveIsPossible e{edir = DUp} l        && not (curdir == DDown)    = e{edir = DUp, emovement = emoveUnit m s DUp}
                                                                           | fst (randomGenerator (lgen l)) < 100 && moveIsPossible e{edir = DDown} l      && not (curdir == DUp)      = e{edir = DDown, emovement = emoveUnit m s DDown}  
                                                                           | otherwise = e


huntgedrag :: Enemy -> LevelData -> Enemy
huntgedrag e@(Enemy{emovement = m, edir = curdir, espeed = s}) l        | fst (randomGenerator (lgen l)) < 30 = randomMoveEnemy e l
                                                                        | myX m >= (pacx l) && moveIsPossible e{edir = DLeft} l      && not (curdir == DRight) && naarx        &&  not rechts    = e{edir = DLeft, emovement = emoveUnit m s DLeft}        --links
                                                                        | myX m <= (pacx l) && moveIsPossible e{edir = DRight} l     && not (curdir == DLeft)  && naarx        &&  not links     = e{edir = DRight, emovement = emoveUnit m s DRight}      --rechts
                                                                        | myY m >= (pacy l) && moveIsPossible e{edir = DUp} l        && not (curdir == DDown)  && not naarx    &&  not onder     = e{edir = DUp, emovement = emoveUnit m s DUp}            --boven
                                                                        | myY m <= (pacy l) && moveIsPossible e{edir = DDown} l      && not (curdir == DUp)    && not naarx    &&  not boven     = e{edir = DDown, emovement = emoveUnit m s DDown}        --onder
                                                                        | otherwise = randomMoveEnemy e l
                  where pacx l      = x $ hpos $ pacmovement $ lPacman $ l
                        pacy l      = y $ hpos $ pacmovement $ lPacman $ l
                        myX m       = x $ hpos $ m
                        myY m       = y $ hpos $ m
                        distx       = abs ((pacx l) - (myX m))
                        distY       = abs ((pacy l) - (myY m))
                        naarx       = (distx > distY)
                        rechts      = (myX m) < (pacx l) 
                        links       = (myX m) > (pacx l)
                        boven       = (myY m) > (pacy l)
                        onder       = (myY m) < (pacy l)


randomMoveEnemy :: Enemy -> LevelData -> Enemy
randomMoveEnemy e@(Enemy{emovement = m, edir = curdir, espeed = s}) l   | moveIsPossible e{edir = DLeft} l      && not (curdir == DRight)  = e{edir = DLeft, emovement = emoveUnit m s DLeft}       --links
                                                                        | moveIsPossible e{edir = DRight} l     && not (curdir == DLeft)   = e{edir = DRight, emovement = emoveUnit m s DRight}      --rechts
                                                                        | moveIsPossible e{edir = DUp} l        && not (curdir == DDown)   = e{edir = DUp, emovement = emoveUnit m s DUp}         --boven
                                                                        | moveIsPossible e{edir = DDown} l      && not (curdir == DUp)   = e{edir = DDown, emovement = emoveUnit m s DDown}         --onder
                                                                        | otherwise = e                      
            
moveIsPossible :: Enemy -> LevelData -> Bool
moveIsPossible e@(Enemy{espeed = s, emovement = m, edir = dir, enextdir = nextdir}) l = not (checkEnemyInAnyWall (checkEnemyInWall e{emovement = enextUnit m s dir} l))

nextEnMove :: Movement -> Speed -> Direction -> Movement
nextEnMove m s dir      | dir == DLeft         = m{npos = Position{x = (xh - s) , y = yh}}
                        | dir == DRight        = m{npos = Position{x = (xh + s) , y = yh}}
                        | dir == DUp           = m{npos = Position{x = xh , y = (yh - s)}}
                        | dir == DDown         = m{npos = Position{x = xh , y = (yh + s)}}
                        | otherwise              = m{npos = Position{x = xh , y = yh}}
                  where xh = x $ hpos $ m
                        yh = y $ hpos $ m                                                                                   
 
checkCross :: Enemy -> LevelData -> Bool
checkCross e@(Enemy{emovement = m, edir = curdir}) l = (length . filter (== True)) (boven : onder: links : rechts : []) >= 2
            where boven =     not (curdir == DDown)      &&    moveIsPossible e{edir = DUp} l
                  onder =     not (curdir == DUp)        &&    moveIsPossible e{edir = DDown} l         
                  links =     not (curdir == DRight)     &&    moveIsPossible e{edir = DLeft} l             --links
                  rechts =    not (curdir == DLeft)      &&    moveIsPossible e{edir = DRight} l             --rechts

randomGenerator :: StdGen -> (Int , StdGen)
randomGenerator rand = randomR (1,100) rand                  

emoveUnit :: Movement -> Speed -> Direction -> Movement
emoveUnit m s dir = m{hpos = (npos $ nextEnMove m s dir), npos = npos $(enextUnit m s dir)}    

enextUnit :: Movement -> Speed -> Direction -> Movement
enextUnit m s dir = m{hpos = (hpos $ m) ,npos = (npos $ nextEnMove (nextEnMove m s dir) s dir)}


{-onTurnPoint :: Enemy -> LevelData -> Bool
onTurnPoint e@(Enemy{emovement = m, espeed = s}) l = (length . filter (== False)) (top : bottom : left : right : []) >= 3
                                    where top         = checkEnemyInAnyWall $ map (boxCollideWalls2 e{emovement = Movement{hpos = Position{x = oldx, y = oldy - s}}}) list
                                          bottom      = checkEnemyInAnyWall $ map (boxCollideWalls2 e{emovement = Movement{hpos = Position{x = oldx, y = oldy + s}}}) list
                                          left        = checkEnemyInAnyWall $ map (boxCollideWalls2 e{emovement = Movement{hpos = Position{x = oldx - s, y = oldy}}}) list
                                          right       = checkEnemyInAnyWall $ map (boxCollideWalls2 e{emovement = Movement{hpos = Position{x = oldx + s, y = oldy}}}) list
                                          list = lwalllist $ l
                                          oldx    = x $ hpos $ m
                                          oldy    = y $ hpos $ m-}                      

                            
checkEnemyInWall :: Enemy -> LevelData -> [Bool]
checkEnemyInWall e l@(LevelData{lwalllist = list}) = map (boxCollideWalls e) list

checkEnemyInAnyWall :: [Bool] -> Bool
checkEnemyInAnyWall []        = False
checkEnemyInAnyWall (b:bs)    | b = True
                              | otherwise = checkEnemyInAnyWall bs

--returns true when it collides with  wall right now
boxCollideWalls2 :: Enemy -> (Int, Int) -> Bool
boxCollideWalls2 e (x,y) = fromIntegral x <= right && fromIntegral x >= left && fromIntegral y <= top && fromIntegral y >= bottom
                  where left              =  a - 0.96
                        right             =  a + 0.96
                        top               =  b + 0.96
                        bottom            =  b - 0.96
                        (Position a b)    =  hpos $ emovement e


boxCollideWalls :: Enemy -> (Int, Int) -> Bool
boxCollideWalls e (x,y) = fromIntegral x <= right && fromIntegral x >= left && fromIntegral y <= top && fromIntegral y >= bottom
                  where left              =  a - 0.98
                        right             =  a + 0.98
                        top               =  b + 0.98
                        bottom            =  b - 0.98
                        (Position a b)    =  npos $ emovement e