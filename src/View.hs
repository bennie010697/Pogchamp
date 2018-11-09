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
    _background <- (drawBackGround levelBoard)
    let _finalBackground = translator (pictures (map pictures (helperPositions 0 _background levelBoard))) levelBoard 
    --let _enemies      =
    --let _ui           = ShowNothing
    --let _player       = 
    let finalPicture    = pictures (_finalBackground : [])
    let scaler  = scale (10 / fromIntegral (noOfColumns levelBoard)) (8 / fromIntegral (noOfRows levelBoard)) finalPicture
    return scaler
    where levelBoard = (lboard $ leveldata gstate)

--_background [[picture]]

makeIOPictures :: [IO Picture] -> IO Picture
makeIOPictures = foldr (\p q -> (<>) <$> p <*> q) (return blank)

drawBackGround :: Board -> IO [[Picture]]
--drawBackGround board = sequence  sequence (positionPlacer 0 (map (map drawField) board) board)
drawBackGround board = sequence $ sequence <$> (map (map drawField) board)

--translator : Positions the given picture correctly on the screen
translator :: Picture -> Board -> Picture
translator p b = translate (0.5 * (fromIntegral (-tileSize * (noOfColumns b)))) (-0.5 * (fromIntegral (-tileSize * (noOfRows b)))) p

-- helperPositions : Determines the actual positions of tiles on the screen
helperPositions :: Int -> [[Picture]] -> Board -> [[Picture]]
helperPositions i p b | i >= (noOfRows b) = []
                      | otherwise         =  map (\x -> Translate (fromIntegral $ fst x) (fromIntegral $ getSingleY i) (snd x)) (zipXwithP i) : helperPositions (i + 1) p b
                    where getAllX n    | n >= (noOfColumns b)     = []
                                       | otherwise                = (n * tileSize) + fst topLeft : getAllX (n + 1)
                          getSingleY n = -((n * tileSize) + snd topLeft)
                          zipXwithP i = zip (getAllX 0) (p !! i)
                                      

-- topLeft : Value of the top left corner in the screen coördinate system 
topLeft :: (Int, Int)
topLeft = (0, 0)
               
-- noOfRows : Returns the number of rows of a board
noOfRows :: Board -> Int
noOfRows b = length b

-- noOfColumns : Returns the number of columns of a board
noOfColumns :: Board -> Int
noOfColumns b = length (head b)


drawField :: Field -> IO Picture
drawField WallField         = getBlueSquare
drawField PowerField        = getBlackSquare
drawField EmptyField        = getBlackSquare
drawField CoinField         = getRedSquare
drawField PacSpawnField     = getBlackSquare
drawField BonusField        = getBlackSquare
drawField EnemySpawnField   = getBlackSquare

{-draw :: Field -> IO Picture
draw f = do
         p <- drawField f-}
         
