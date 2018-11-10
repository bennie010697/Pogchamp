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

data Pacman = Pacman {pacspeed :: Speed, pacmovement :: Movement}

--list of things to find in the world
isPacSpawn :: Field -> Bool
isPacSpawn f = f == PacSpawnField

isPacSpawnLocation :: Board -> (Int, Int)
isPacSpawnLocation b = (x , y)
    where x         = fromJust (elemIndex PacSpawnField (b !! y))
          y         = fromJust (elemIndex (head ([x | x <- b, elem PacSpawnField x])) b)

getPacSpawn :: Board -> Position
getPacSpawn b = Position {x = fromIntegral (fst (isPacSpawnLocation b)), y = fromIntegral (snd (isPacSpawnLocation b))}


--List of pictures needed for games (sprites)


getFruitSquare :: IO Picture
getFruitSquare = loadBMP "pictures/Fruit.bmp"

getBlueSquare :: IO Picture
getBlueSquare = loadBMP "pictures/blueSquare.bmp"

getRedSquare :: IO Picture
getRedSquare = loadBMP "pictures/redSquare.bmp"

getBlackSquare :: IO Picture
getBlackSquare = loadBMP "pictures/blackSquare.bmp"

getGameOver :: IO Picture
getGameOver = loadBMP "pictures/GameOver.bmp"

getMainMenuBackground :: IO Picture
getMainMenuBackground = loadBMP "pictures/MainMenu.bmp"

getPacMan1 :: IO Picture
getPacMan1 = loadBMP "pictures/pacman.bmp"




