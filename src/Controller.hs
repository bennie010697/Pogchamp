-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Variables
import Level
import Pacman

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
step _ gstate | state gstate == Level = if checkNoFruit (lfruitlist $ leveldata $ gstate) then return $ (EndScreenState{state = EndScreen, eindscore = score})  else return $ handleLevelLoop gstate 
              | otherwise = return $ gstate
              where score = lScore $ leveldata $ gstate


handleLevelLoop :: GameState -> GameState
handleLevelLoop gstate = gstate {leveldata = makePacMove  $ updateFruits (leveldata gstate)}

checkNoFruit :: [(Int, Int)] -> Bool
checkNoFruit [] = True
checkNoFruit _  = False

    -- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate | state gstate == Menu = menuHelpKeys e gstate
               | state gstate == Level = return (levelHelpKeys e gstate)
               | otherwise = return gstate
                
menuHelpKeys :: Event -> GameState -> IO GameState
menuHelpKeys (EventKey (SpecialKey c) Down _ _) gstate
             | c == KeyEnter  = do
                                startlvl <- makeNewLevel
                                let state = LevelState Level startlvl (sprites gstate)
                                return state
             |otherwise       = return gstate
menuHelpKeys _ gstate = return gstate

levelHelpKeys :: Event -> GameState -> GameState
levelHelpKeys (EventKey (SpecialKey c) Down _ _) gstate
              | c == KeyLeft    = LevelState {state = Level, leveldata = nextmove (leveldata gstate) DLeft, sprites = sprites gstate}
              | c == KeyRight   = LevelState {state = Level, leveldata = nextmove (leveldata gstate) DRight, sprites = sprites gstate}
              | c == KeyUp      = LevelState {state = Level, leveldata = nextmove (leveldata gstate) DUp, sprites = sprites gstate}
              | c == KeyDown    = LevelState {state = Level, leveldata = nextmove (leveldata gstate) DDown, sprites = sprites gstate}
              | c == KeyEsc     = MenuState  {state = Menu, background = getMainMenuBackground, sprites = sprites gstate}
              | otherwise       = gstate
levelHelpKeys _ gstate = gstate

nextmove :: LevelData -> Direction -> LevelData
nextmove leveldata dir = leveldata{lNextMove = dir}

makeNewLevel :: IO LevelData
makeNewLevel = do
               levelBoard <- loadLevelFromFile
               let fruitlijst = getFruitList 0 levelBoard
               let state = LevelData{lboard = levelBoard, lNextMove = DUp, lPacman = newPacman levelBoard, ltick = True, lfruitlist = fruitlijst, lScore = 0}
               return state
  

{-inputKey :: Event -> GameState -> GameState
inputKey e gstate = case state gstate of
            _ -> -}