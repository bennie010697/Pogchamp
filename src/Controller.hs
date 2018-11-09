-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Variables
import Level

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Char

-- | Handle One Iteration of the game

{-step :: Float -> GameState -> IO GameState
step secs gstate
  | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
  = -- We show a new random number
    do randomNumber <- randomIO
       let newNumber = abs randomNumber `mod` 10
       return $ GameState (ShowANumber newNumber) 0
  | otherwise
  = -- Just update the elapsed time
    return $ gstate { elapsedTime = elapsedTime gstate + secs }-}

step :: Float -> GameState -> IO GameState
step _ gstate = return $ gstate


    -- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate | state gstate == Menu = menuHelpKeys e gstate
               | otherwise = return gstate
                
menuHelpKeys :: Event -> GameState -> IO GameState
menuHelpKeys (EventKey (SpecialKey c) Down _ _) gstate
             | c == KeyEnter  = do
                                startlvl <- makeNewLevel
                                let state = LevelState Level startlvl
                                return state
             |otherwise       = return gstate

menuHelpKeys _ gstate = return gstate

makeNewLevel :: IO LevelData
makeNewLevel = do
               levelBoard <- loadLevelFromFile
               let state = LevelData levelBoard
               return state
               
{-inputKey :: Event -> GameState -> GameState
inputKey e gstate = case state gstate of
            _ -> -}