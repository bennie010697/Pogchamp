-- | This module defines how to turn
--   the game state into a picture
module View where

import Data.Monoid((<>))

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@(GameState gf@(GameField field _ _) time _) = case field of
  ( Grid [Row [Empty]] [Row [Empty]] ) -> viewField gf
  otherwise -> viewField gf
  
viewField :: GameField -> Picture
viewField (GameField (Grid rows columns) upL downR) = Color green $ viewGrid upL downR (length rows) (length columns)

viewGrid :: Point -> Point -> Int -> Int -> Picture
viewGrid upLeft@(u,v) downRight@(z,w) cellsH cellsV = linesHor <> linesVert
  where linesHor   = foldr (\x -> mappend (Line x) ) blank [ [ (u, y), (z, y) ] | y <- [v, v - yStep .. w] ]
        linesVert  = foldr (\x -> mappend (Line x) ) blank [ [ (x, v), (x, w) ] | x <- [u, u - xStep .. z] ]
        xStep = (u - z) / (fromIntegral cellsH)
        yStep = (v - w) / (fromIntegral cellsV)
{-
  case elapsedTime gstate of
  otherwise -> color green (text "hey")
-}