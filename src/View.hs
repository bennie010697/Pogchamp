-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model
import Variables

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

drawBackGround :: Board -> IO Picture
drawBackGround board = do 
                       let pics = map (map drawField) board
                       return $ pictures (map pictures (map pictures pics))
                  


drawField :: Field -> IO Picture
drawField WallField         = getBlueSquare
drawField PowerField        = return Blank
drawField EmptyField        = return Blank
drawField CoinField         = getRedSquare
drawField PacSpawnField     = return Blank
drawField BonusField        = return Blank
drawField EnemySpawnField   = return Blank
