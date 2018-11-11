-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Variables
import Data.Monoid ((<>))
import Control.Monad
import Data.Foldable

view :: GameState -> IO Picture
view gstate = case state gstate of
  Menu    -> getMainMenuBackground
  Paused  -> getPauseBackground
  EndScreen -> do
    _gameoverpic <- getGameOver
    return $ pictures (_gameoverpic : (translate (-250) (250) (color red (text ("Score: " ++ show (eindscore $ gstate ))))):  [])
    
  Level   -> do
    _background   <- (drawBackGround levelBoard gstate)
    let _finalBackground = translator (pictures (map pictures (helperPositions 0 _background levelBoard))) levelBoard 
    let _ui           = translator (drawUIScore gstate) levelBoard
    _fruithappjes <- drawFruits gstate
    let _finalfruithappjes = translator (pictures _fruithappjes) levelBoard 
    _player       <- (drawPacMan gstate (leveldata gstate))
    let _finalplayer     = translator _player levelBoard 
    _Enemy        <- (drawEnemies (leveldata gstate))
    let finalEnemy      = translator _Enemy levelBoard
    let finalPicture    = pictures (_finalBackground : _finalplayer : _finalfruithappjes : finalEnemy : _ui : [])
    let scaler  = scale (10 / fromIntegral (noOfColumns levelBoard)) (8 / fromIntegral (noOfRows levelBoard)) finalPicture
    return scaler
    where levelBoard = (lboard $ leveldata gstate)

--_background [[picture]]
drawUIScore :: GameState -> Picture
drawUIScore g = translate 50 (-150) (color red (text ("Score: " ++ show (lScore $ leveldata $ g))))

drawEnemies :: LevelData -> IO Picture
drawEnemies LevelData{lenemylist = l} = do
                              _picturelist <- drawEnemiesRecursion l
                              return $ pictures $ _picturelist
                      
drawEnemiesRecursion :: [Enemy] -> IO [Picture]
drawEnemiesRecursion [] = return []
drawEnemiesRecursion (z:zs) = do 
                      _enemyPicture <- getRedSquare
                      _recursivePicture <- drawEnemiesRecursion zs
                      let _pictures = (translate ((fromIntegral tileSize) * x) ((fromIntegral tileSize) * (-y)) _enemyPicture) : _recursivePicture
                      return $ _pictures
                  where x = getXLocation (emovement z)
                        y = getYLocation (emovement z)

drawPacMan :: GameState -> LevelData -> IO Picture
drawPacMan g LevelData{lPacman = pacman, ltick = tick} = do 
                    _pacpicture <- getPic tick g
                    return (translate x y _pacpicture)
          where x = (fromIntegral tileSize) * (getXLocation (pacmovement pacman))
                y = (fromIntegral tileSize) * (-(getYLocation (pacmovement pacman)))

getPic :: Bool -> GameState -> IO Picture
getPic True g = sprites g !! 4
getPic False g = sprites g !! 5

drawFruits :: GameState ->  IO [Picture]
drawFruits g@(LevelState{leveldata = ldata}) = do
                  _fruitsprite <- (sprites g !! 0)
                  return $ (flip draw _fruitsprite) <$> list
            where draw (x,y) = Translate (x * fromIntegral tileSize) ((-y) * fromIntegral tileSize)
                  list = (\(x,y) -> (fromIntegral x, fromIntegral y)) <$> lfruitlist ldata
                  

getXLocation :: Movement -> Float
getXLocation m  = x (hpos m)

getYLocation :: Movement -> Float
getYLocation m =  y (hpos m)

makeIOPictures :: [IO Picture] -> IO Picture
makeIOPictures = foldr (\p q -> (<>) <$> p <*> q) (return blank)

drawBackGround :: Board -> GameState -> IO [[Picture]]
drawBackGround board g = sequence $ sequence <$> (map (map (drawField g)) board)

--translator : Positions the given picture correctly on the screen
translator :: Picture -> Board -> Picture
translator p b = translate (0.5 * (fromIntegral (-tileSize * (noOfColumns b)))) (-0.5 * (fromIntegral (-tileSize * (noOfRows b)))) p

-- helperPositions : Determines the actual positions of tiles on the screen
helperPositions :: Int -> [[Picture]] -> Board -> [[Picture]]
helperPositions i p board | i >= (noOfRows board) = []
                          | otherwise         =  map (\x -> Translate (fromIntegral $ fst x) (fromIntegral $ getSingleY i) (snd x)) (zipXwithP i) : helperPositions (i + 1) p board
                    where getxList n    | n >= (noOfColumns board)      = []
                                        | otherwise                     = (n * tileSize) + fst topLeft : getxList (n + 1)
                          getSingleY n  = -((n * tileSize) + snd topLeft)
                          zipXwithP i   = zip (getxList 0) (p !! i)
                                      

-- topLeft : Value of the top left corner in the screen coördinate system 
topLeft :: (Int, Int)
topLeft = (0, 0)
               
-- noOfRows : Returns the number of rows of a board
noOfRows :: Board -> Int
noOfRows b = length b

-- noOfColumns : Returns the number of columns of a board
noOfColumns :: Board -> Int
noOfColumns b = length (head b)

--drawField : draws the appropiate picture for a field
drawField :: GameState -> Field -> IO Picture
drawField g WallField       = sprites g !! 1
drawField g PowerField      = sprites g !! 3
drawField g EmptyField      = sprites g !! 3
drawField g CoinField       = sprites g !! 3 
drawField g PacSpawnField   = sprites g !! 3
drawField g BonusField      = sprites g !! 3
drawField g EnemySpawnField = sprites g !! 3
       
