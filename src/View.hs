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
  --Level   -> return $ ShowNothing
    
    --let  
