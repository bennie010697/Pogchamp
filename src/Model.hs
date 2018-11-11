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
import Variables


data StateName = Menu | Paused | Level | EndScreen
                 deriving (Eq)

nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = MenuState { state :: StateName , background :: IO Picture, sprites :: [IO Picture]}
                 | PausedState { state :: StateName , background :: IO Picture, sprites :: [IO Picture]}
                 | LevelState { state :: StateName , leveldata :: LevelData, sprites :: [IO Picture]}
                 | EndScreenState {state :: StateName, eindscore :: Int}

data LevelData = LevelData {lboard :: Board, lNextMove :: Direction, lPacman :: Pacman, ltick :: Bool, lfruitlist :: [(Int, Int)], lScore :: Int}              
initialState :: GameState
initialState = MenuState {state = Menu, background = getMainMenuBackground, sprites = getSprites}

getSprites :: [IO Picture]
getSprites = getFruitSquare : getBlueSquare : getRedSquare : getBlackSquare : getPacMan1 : getPacMan2 : []