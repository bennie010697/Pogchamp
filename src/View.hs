-- | This module defines how to turn
--   the game state into a picture
module View where

import Data.Monoid((<>))
import Control.Applicative (ZipList(..))

import Graphics.Gloss
import Model

view :: GameState -> ViewData -> IO Picture
view gs vd = return $ viewPure gs vd
--view = \p -> loadBMP "E:/University/Haskell_Projects/PacMan/Pictures/pocman.bmp"

viewPure :: GameState -> ViewData -> Picture
viewPure gstate@(GameState gf@(GameField field _ _) time _) vData = case field of
  ( Grid [Row [Empty]] [Row [Empty]] ) -> viewField gf -- Do something with Empty board?
  otherwise -> viewField gf <> viewSprites gf vData
  
-- Creates a grid based on the input level grid
viewField :: GameField -> Picture
viewField (GameField (Grid rows columns) upL downR) = Color green $ viewGrid upL downR (length rows) (length columns)

viewGrid :: Point -> Point -> Int -> Int -> Picture
viewGrid upLeft@(u,v) downRight@(z,w) cellsH cellsV = linesHor <> linesVert
  where linesHor   = foldr (\x -> mappend (Line x) ) blank [ [ (u, y), (z, y) ] | y <- [v, v - yStep .. w] ]
        linesVert  = foldr (\x -> mappend (Line x) ) blank [ [ (x, v), (x, w) ] | x <- [u, u - xStep .. z] ]
        xStep = (u - z) / (fromIntegral cellsH)
        yStep = (v - w) / (fromIntegral cellsV)

-- Puts sprites onto the level grid
viewSprites :: GameField -> ViewData -> Picture
viewSprites (GameField (Grid rows _) origin _) vData = let viewRow = flip (viewSpriteRow $ drawSprite vData)
                                              in Pictures $ getZipList $ ZipList (viewRow
                                              <$> [ (0.0, (fromIntegral y) * 10.0) | y <- [0 .. (length rows) - 1] ] ) 
                                              <*> ZipList (getRow <$> rows)
-- ZipList to ensure row 1 gets matched to offset 1 (and not all combinations)

-- Add the offsets to the sprites in a row (recursively)
viewSpriteRow :: (Point -> Field -> Picture) -> [Field] -> Point -> Picture
viewSpriteRow _ [] _                = blank
viewSpriteRow drawS [f] p           = drawS p f
viewSpriteRow drawS (f:fs) p@(x,y)  = (drawS p f) <> (viewSpriteRow drawS fs (x + 10.0, y))

drawSprite :: ViewData -> Point -> Field -> Picture
drawSprite vData (x,y) = (Translate x y) . (flip selectSprite vData)

-- link the correct sprite from ViewData to the right field
selectSprite :: Field -> ViewData -> Picture
selectSprite Player  = pacMan
selectSprite Ghost   = ghost
selectSprite Pellet  = pellet
selectSprite PowerUp = powerUp
selectSprite _       = \v -> blank
