-- | This module contains the data types
--   which represent the state of the game
module Model where

import Variables 
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.Pure.Game
import Data.List
import Data.Maybe
import System.Random


data StateName = Menu | Paused | Level
                 deriving (Eq)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = MenuState { state :: StateName , background :: IO Picture}
                 | PausedState { state :: StateName , background :: IO Picture}
                 | LevelState { state :: StateName , leveldata :: LevelData}

data LevelData = LevelData {lboard :: Board}              
initialState :: GameState
initialState = MenuState {state = Menu, background = getMainMenuBackground}