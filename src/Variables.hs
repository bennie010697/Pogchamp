module Variables where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap

-- Field : Enum of things that could be on the board
data Field = WallField | PowerField | EmptyField | CoinField | PacSpawnField | BonusField |EnemySpawnField
            deriving (Show, Eq)

-- Row : A row of fields
type Row = [Field]

-- Board : A list of rows
type Board = [Row]


--List of pictures needed for games (sprites)
getGreenSquare :: IO Picture
getGreenSquare = loadBMP "pictures/greenSquare.png"

getBrownSquare :: IO Picture
getBrownSquare = loadBMP "pictures/brownSquare.png"

getGraySquare :: IO Picture
getGraySquare = loadBMP "pictures/graySquare.png"

getBlueSquare :: IO Picture
getBlueSquare = loadBMP "pictures/blueSquare.png"

getRedSquare :: IO Picture
getRedSquare = loadBMP "pictures/redSquare.png"

getGameOver :: IO Picture
getGameOver = loadBMP "pictures/GameOver.bmp"

getMainMenuBackground :: IO Picture
getMainMenuBackground = loadBMP "pictures/MainMenu.bmp"

