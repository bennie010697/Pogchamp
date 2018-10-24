-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss.Data.Display (Display(..))
import Graphics.Gloss.Data.Point (Point)

newtype Row a = Row [a]
type Column a = Row a
data Grid a = Grid [Row a] [Column a] | Grid_1D [Row a] deriving Show

data Portal = Ref Portal | Val Field
data Field = Empty
            | Wall
            | Portal 
            | Player
            | Ghost
            | Fruit
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

initialFieldState :: Grid Field
initialFieldState = createGrid Empty 10 10

initialState :: (Display -> GameState)
initialState = GameState (GameField initialFieldState (200,200) (-200,-200)) 0