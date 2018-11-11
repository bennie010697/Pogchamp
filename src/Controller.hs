-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model
import Variables
import Level
import Pacman
import Enemy

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Data.List
import Data.Char
import System.Directory 

-- | Handle One Iteration of the game

step :: Float -> GameState -> IO GameState
step _ gstate | state gstate == Level = if checkNoFruit (lfruitlist $ leveldata $ gstate) then return $ (EndScreenState{state = EndScreen, eindscore = score, sprites = sprites gstate})  else if checknolives (llives $ leveldata $ gstate) then return $ (EndScreenState{state = EndScreen, eindscore = score, sprites = getSprites})  else return $ handleLevelLoop gstate 
              | state gstate == EndScreen = do
                                            _ <- createTempFile (eindscore gstate) 
                                            _ <- createHighscoreFile
                                            return gstate                                
              | otherwise = return $ gstate
              where score = lScore $ leveldata $ gstate

checknolives :: Int -> Bool
checknolives i | i < 0 = True
               | otherwise = False

handleLevelLoop :: GameState -> GameState
handleLevelLoop gstate = gstate {leveldata = makePacMove  $ updateFruits $ changeldirection $ pacmanDies $ moveEnemies (leveldata gstate)}

checkNoFruit :: [(Int, Int)] -> Bool
checkNoFruit [] = True
checkNoFruit _  = False



    -- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate | state gstate == Menu       = menuHelpKeys e gstate
               | state gstate == Level      = return (levelHelpKeys e gstate)
               | state gstate == EndScreen  = return (endScreenKeys e gstate)
               | state gstate == Paused     = return (pausedScreenKeys e gstate)
               | otherwise = return gstate 
 
endScreenKeys :: Event -> GameState -> GameState
endScreenKeys (EventKey (SpecialKey c) Down _ _) gstate
                | c == KeyEsc     = MenuState  {state = Menu, background = getMainMenuBackground, sprites = sprites gstate}
                | otherwise = gstate
endScreenKeys _ gstate = gstate

pausedScreenKeys :: Event -> GameState -> GameState
pausedScreenKeys (EventKey (SpecialKey c) Down _ _) gstate
                | c == KeyF1      = LevelState {state = Level, leveldata = leveldata gstate, sprites = getSprites}
                | c == KeyEsc     = MenuState  {state = Menu, background = getMainMenuBackground, sprites = sprites gstate}
                | otherwise = gstate
pausedScreenKeys _ gstate = gstate     

menuHelpKeys :: Event -> GameState -> IO GameState
menuHelpKeys (EventKey (SpecialKey c) Down _ _) gstate
             | c == KeyEnter  = do
                                startlvl <- makeNewLevel
                                let state = LevelState Level startlvl (getSprites)
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
              | c == KeyF1      = PausedState {state = Paused, background = getPauseBackground, sprites = sprites gstate, leveldata = (leveldata gstate)}
              | otherwise       = gstate
levelHelpKeys _ gstate = gstate

nextmove :: LevelData -> Direction -> LevelData
nextmove leveldata dir = leveldata{lNextMove = dir}

makeNewLevel :: IO LevelData
makeNewLevel = do
               levelBoard <- loadLevelFromFile
               random <- getStdGen
               let fruitlijst = getFruitList 0 levelBoard
               let walllijst  = getWallList 0 levelBoard
               let enemylijst = [newEnemy levelBoard Hunts 0, newEnemy levelBoard Hunts 1, newEnemy levelBoard MovesRandom 2, newEnemy levelBoard MovesRandom 3]
               let state = LevelData{lboard = levelBoard, lNextMove = DRight, ldirection = DRight, lPacman = newPacman levelBoard, ltick = True, lfruitlist = fruitlijst, lScore = 0, lwalllist = walllijst, lenemylist = enemylijst, llives = 3, lgen = random}
               return state

-- createTempFile : Reads the data from the highscore file, computes it, and writes the result to a temporary file
createTempFile :: Int -> IO ()
createTempFile h = do
                   values <- readFile "highscore.txt"
                   let nr = makeNr (reverse values)
                   let hs = if read nr > h then read nr else h
                   writeFile "temp.txt" (getString hs)
                   where getString hs = "Highscore : " ++ show hs

makeNr :: String -> String
makeNr []      = []
makeNr (x:xs)  | isDigit x    = makeNr xs ++ [x]
               | otherwise = []

               -- createHighscoreFile : Reads the data from the temporary file, and writes it to the highscore file. This needs to be done in order to dodge the read/write lock                        
createHighscoreFile :: IO ()
createHighscoreFile = do
                      file <- readFile "temp.txt"
                      writeFile "highscore.txt" file
                      removeFile "temp.txt"