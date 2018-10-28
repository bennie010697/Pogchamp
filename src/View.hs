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
    _background <- drawBackGround (lboard $ leveldata gstate)
    --let _enemies      =
    --let _ui           = ShowNothing
    --let _player       = 
    return $ pictures (_background : [])

{-drawBackGround :: Board -> IO Picture
drawBackGround board = do 
                       let pics = map (map drawField) board
                       (foldr (\p q -> p >>= (\p' -> q >>= (return . ((<>) p')) )) (return blank)) (map (foldr (\p q -> p >>= (\p' -> q >>= (return . ((<>) p')) )) (return blank)) pics)
-}

makeIO :: [IO Picture] -> IO Picture
makeIO = foldr (\p q -> (<>) <$> p <*> q) (return blank)

drawBackGround :: Board -> IO Picture
drawBackGround board = makeIO $ makeIO <$> (map drawField) <$> board

drawField :: Field -> IO Picture
drawField WallField         = getBlueSquare
drawField PowerField        = return Blank
drawField EmptyField        = return Blank
drawField CoinField         = getRedSquare
drawField PacSpawnField     = return Blank
drawField BonusField        = return Blank
drawField EnemySpawnField   = return Blank

{-draw :: Field -> IO Picture
draw f = do
         p <- drawField f-}
         
