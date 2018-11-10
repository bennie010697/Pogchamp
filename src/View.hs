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
  Paused  -> getGameOver
  Level   -> do
    _background   <- (drawBackGround levelBoard)
    let _finalBackground = translator (pictures (map pictures (helperPositions 0 _background levelBoard))) levelBoard 
    --let _enemies      =
    --let _ui           = ShowNothing
    _player       <- (drawPacMan (leveldata gstate))
    let _finalplayer     = translator _player levelBoard
    let finalPicture    = pictures (_finalBackground : _finalplayer : [])
    let scaler  = scale (10 / fromIntegral (noOfColumns levelBoard)) (8 / fromIntegral (noOfRows levelBoard)) finalPicture
    return scaler
    where levelBoard = (lboard $ leveldata gstate)

--_background [[picture]]

drawPacMan :: LevelData -> IO Picture
drawPacMan LevelData{lPacman = pacman} = do 
                    _pacpicture <- getPacMan1
                    return (translate x y _pacpicture)
          where x = (fromIntegral tileSize) * (getXLocation (pacmovement pacman))
                y = (fromIntegral tileSize) * (-(getYLocation (pacmovement pacman)))
                    

getXLocation :: Movement -> Float
getXLocation m  = x (hpos m)

getYLocation :: Movement -> Float
getYLocation m =  y (hpos m)

makeIOPictures :: [IO Picture] -> IO Picture
makeIOPictures = foldr (\p q -> (<>) <$> p <*> q) (return blank)

drawBackGround :: Board -> IO [[Picture]]
drawBackGround board = sequence $ sequence <$> (map (map drawField) board)

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
drawField :: Field -> IO Picture
drawField WallField         = getBlueSquare
drawField PowerField        = getBlackSquare
drawField EmptyField        = getBlackSquare
drawField CoinField         = head Sprites
drawField PacSpawnField     = getBlackSquare
drawField BonusField        = getBlackSquare
drawField EnemySpawnField   = getBlackSquare
       
