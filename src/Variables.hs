module Variables where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Data.List
import Data.Maybe


-- Field : Enum of things that could be on the board
data Field = WallField | PowerField | EmptyField | CoinField | PacSpawnField | BonusField |EnemySpawnField
            deriving (Show, Eq)


-- Speed : the speed at which things move
type Speed = Float            
-- Row : A row of fields
type Row = [Field]
-- Board : A list of rows
type Board = [Row]

--tileSize : A size of tiles of the level.
tileSize :: Int
tileSize = 100

-- Movement : The first position is the current position, the second position is the next position
data Movement = Movement {hpos :: Position, npos :: Position}

-- Position : X and Y position.
data Position = Position {x :: Float, y :: Float}
            deriving (Show, Eq)

data Direction = DLeft | DUp | DRight | DDown
                deriving (Show, Eq)

data Pacman = Pacman { pacspeed :: Speed
                     , pacmovement :: Movement
                     }

data EnemyType = Blinky -- rood  - nadert Pacman en volgt daarna spoor Pacman
               | Pinky  -- roze  - zoekt route naar blokje naast Pacman
               | Inky   -- blauw - beslist iedere ronde of hij naar Pacman toe loopt of van hem af
               | Clyde  -- geel  - nadert Pacman en zoekt daarna linkerbenedenhoek (x = 0, y = board.height)

data EnemyState = Hunts
                | MovesRandom
                deriving (Show, Eq)  

data Enemy = Enemy { etype     :: EnemyType
                   , estate    :: EnemyState
                   , espeed    :: Speed
                   , emovement :: Movement
                   , edir      :: Direction
                   , enextdir  :: Direction
                   }

--list of things to find in the world
isPacSpawn :: Field -> Bool
isPacSpawn f = f == PacSpawnField

isPacSpawnLocation :: Board -> (Int, Int)
isPacSpawnLocation b = (x , y)
    where x         = fromJust (elemIndex PacSpawnField (b !! y))
          y         = fromJust (elemIndex (head ([x | x <- b, elem PacSpawnField x])) b)

getPacSpawn :: Board -> Position
getPacSpawn b = Position {x = fromIntegral (fst (isPacSpawnLocation b)), y = fromIntegral (snd (isPacSpawnLocation b))}

isEnemySpawn :: Field -> Bool
isEnemySpawn f = f == EnemySpawnField

isEnemySpawnLocation :: Board -> (Int, Int)
isEnemySpawnLocation b = (x, y)
    where x = fromJust (elemIndex EnemySpawnField (b !! y))
          y = fromJust (elemIndex (head ([x | x <- b, elem EnemySpawnField x])) b)

getEnemySpawn :: Board -> Int -> Position
getEnemySpawn b i= Position {x = fromIntegral ((fst (isEnemySpawnLocation b)) + i), y = fromIntegral (snd (isEnemySpawnLocation b))}

--List of pictures needed for games (sprites)


getFruitSquare :: IO Picture
getFruitSquare = return $ translate 50 50 $ Color yellow $ circleSolid 20

getBlueSquare :: IO Picture
getBlueSquare = return $ Color blue $ polygon [(0,0),(100,0),(100,100),(0,100)]

getRedSquare :: IO Picture
getRedSquare = return $ Color red $ polygon [(0,0),(100,0),(100,100),(0,100)]

getBlackSquare :: IO Picture
getBlackSquare = return $ Color black $ polygon [(0,0),(100,0),(100,100),(0,100)]

getGameOver :: IO Picture
getGameOver = loadBMP "pictures/GameOver.bmp" 

getMainMenuBackground :: IO Picture
getMainMenuBackground = loadBMP "pictures/MainMenu.bmp"

getPauseBackground :: IO Picture
getPauseBackground = loadBMP "pictures/Paused.bmp"

getPacMan1 :: IO Picture
getPacMan1 = --loadBMP "pictures/pacman.bmp"
            return $ pictures $ Color black (polygon [(0,0),(100,0),(100,100),(0,100)]) : translate 50 50 (Color yellow (circleSolid 30)) : Color black (polygon [(50,50),(90,60),(90,40)])  : []

getPacMan2 :: IO Picture
getPacMan2 = return $ pictures $ Color black (polygon [(0,0),(100,0),(100,100),(0,100)]) : translate 50 50 (Color yellow (circleSolid 30)) : Color black (polygon [(50,50),(90,90),(90,10)])  : []




