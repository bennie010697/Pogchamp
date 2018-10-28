-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Data.Display (Display(..))
import Graphics.Gloss.Data.Point (Point)
import Graphics.Gloss.Data.Picture(Picture(..))

newtype Row a = Row {getRow :: [a]}
type Column a = Row a
data Grid a = Grid [Row a] [Column a] | Grid_1D [Row a] deriving Show

data Portal = Ref Portal | Val Point deriving (Show)
data Field = Empty
            | Wall
            | Field Portal 
            | Player
            | Ghost
            | Pellet
            | PowerUp
            deriving (Show)
          
data GameField = GameField {
                  grid :: Grid Field
                , upLeft :: Point
                , downRight :: Point
}
data GameState = GameState {
                   gameField :: GameField
                 , elapsedTime :: Float
                 , windowType :: Display
                 }
data ViewData = ViewData {
                    pacMan :: Picture
                  , ghost :: Picture
                  , pellet :: Picture
                  , powerUp :: Picture
}

instance Show a => Show (Row a) where
 show (Row as) = concat $ map (\x -> show x ++ " ") as
                
createGrid :: Show a => a -> Int -> Int -> Grid a
createGrid initVal width height = let rep w h = map Row . replicate w . replicate h 
                                  in Grid (rep height width initVal) (rep width height initVal)

prettyPrintGrid :: Show a => Grid a -> IO()
prettyPrintGrid (Grid rows columns) = let print = mapM_ putStrLn . map (\x -> "|" ++ show x ++ "|")
                                      in putStrLn "Rows" >> print rows >> putStrLn "Columns"  >> print columns

templateWindowed :: Display
templateWindowed = (InWindow "template" (0,0) (0,0))

templateFieldState :: Grid Field
templateFieldState = createGrid Empty 10 10

initialState :: Grid Field -> Display -> GameState
initialState grid = GameState (GameField grid (200,200) (-200,-200)) 0
