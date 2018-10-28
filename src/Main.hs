{-# LANGUAGE OverloadedStrings #-}
module Main where

import Controller (step, input)
import Model (initialState, prettyPrintGrid, Row(..), Grid(..), Field(..), Portal(..), ViewData(..))
import View (view)

import System.IO
import System.Directory(getAppUserDataDirectory, createDirectoryIfMissing)
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment (getArgs, getProgName)
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Either (Either(..))
import Control.Monad (sequence)

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Display (Display(..))
import Graphics.Gloss.Data.Bitmap (loadBMP, bitmapDataOfBMP )
import Codec.BMP (unpackBMPToRGBA32, BMP(..))

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName

    -- Set up appData
    dir <- getAppUserDataDirectory "PacMan" 
    createDirectoryIfMissing False dir

    -- Load in Grid
    mapM_ putStrLn args
    let path = dir ++ "/grid.txt"
    lines <- B.lines <$> B.readFile path
    let g = rowsToGrid lines
    prettyPrintGrid g

    -- Load in BMP sprites
    vData <- getSprites $ dir ++ "/Sprites"
    let run = main' g vData

    case args of
        ["Windowed"]    -> putStrLn "Running in Windowed mode" >> run (InWindow "PagChomp" (400, 400) (0, 0)) -- size , position
        ["FullScreen"]  -> putStrLn "Running in FullScreen mode" >> run FullScreen
        _               -> putStrLn $ "USAGE: " ++ prog ++ " [Windowed|FullScreen]"

main' :: Grid Field -> ViewData -> Display -> IO ()
main' g vData dis = playIO dis
    black               -- Background color
    30                  -- Frames per second
    (initialState g dis)-- Initial state
    (flip view vData)        -- View function
    input               -- Event function
    step                -- Step function

-- Retrieve Grid from textFile via lazy ByteStrins
rowsToGrid :: [B.ByteString] -> Grid Field
rowsToGrid bs = Grid rows columns
        where rows      = map lineToRow bs
              columns   = map lineToRow $ B.transpose bs

lineToRow :: B.ByteString -> Row Field
lineToRow = Row . mapMaybe getField . B.split ' '

getField :: B.ByteString -> Maybe Field
getField bs = case bs of
    "E" -> Just Empty
    "W" -> Just Wall
    "P" -> Just Player
    "G" -> Just Ghost
    "*" -> Just Pellet
    "+" -> Just PowerUp
    empty -> Nothing
    otherwise -> case B.head bs of
        '|' -> Just $ Field $ Ref $ Val (fromIntegral x, fromIntegral y)                   -- should be entered as: "|10_10" 
        otherwise -> Nothing
    where Just(x,_) = B.readInt $ B.tail bs             -- Read the first Int
          Just(y,_) = B.readInt $ B.tail $ B.tail bs    -- 

-- Retrieve and store sprites
getSprites :: FilePath -> IO ViewData
getSprites path = let getSprite s = loadBMP $ path ++ "/" ++ s ++ ".bmp" 
    in do
        [pMan, g, pel, pUp] <- sequence $ getSprite <$> ["pocman", "ghost", "pellet", "powerUp"]
        return $ ViewData {
            pacMan = pMan,
            ghost = g,
            pellet = pel,
            powerUp = pUp
        }
    -- hardcoded file names

getSpriteSheet :: FilePath -> IO BitmapData
getSpriteSheet path = do 
    -- add isLeft check for error?
    bmp <- Right readBMP path
    let bmpData = bitmapDataOfBMP bmp
