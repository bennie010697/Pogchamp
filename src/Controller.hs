-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model(prettyPrintGrid, templateWindowed, GameState(..))
import Graphics.Gloss.Data.Display (Display(..))


--import Graphics.Rendering.OpenGL

import Graphics.UI.GLUT.Window(leaveFullScreen, fullScreen, windowTitle)
--import Graphics.UI.GLUT.Window (leaveFullScreen)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Display (Display(..))
import System.Random
import Data.StateVar

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate@(GameState field _ _) = return gstate

{-
  | (round $ elapsedTime gstate + secs) `mod` 60 == 0
  = -- We show a new random number
    prettyPrintGrid field >> (return $ gstate { elapsedTime = elapsedTime gstate + secs } )
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }
-}
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = inputKey e gstate

inputKey :: Event -> GameState -> IO GameState
inputKey (EventKey (SpecialKey KeyEsc) _ _ _) gstate = handleESC gstate
inputKey (EventKey (SpecialKey KeyF5) _ _ _) gstate  = handleF5 gstate
inputKey _ gstate = return gstate -- Otherwise keep the same

handleESC :: GameState -> IO GameState
handleESC gstate@(GameState _ _ FullScreen) = leaveFullScreen >> 
                                                (windowTitle $= "PogChamp")  >>
                                                (return $ gstate {windowType = (templateWindowed)})
handleESC gstate                              = return gstate

handleF5 :: GameState -> IO GameState
handleF5 gstate@(GameState _ _ (InWindow _ _ _ ))  = fullScreen >>
                                                      (return $ gstate {windowType = FullScreen})
handleF5 gstate                                     = return gstate
