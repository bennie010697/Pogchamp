module Main where

import Controller (step, input)
import Model (initialState)
import View (view)

import System.Environment (getArgs, getProgName)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Display (Display(..))

main :: IO ()
main = do
    args <- getArgs
    prog <- getProgName
    case args of
        ["Windowed"]    -> putStrLn "Running in Windowed mode" >> main' (InWindow "PagChomp" (400, 400) (0, 0)) -- size , position
        ["FullScreen"]  -> putStrLn "Running in FullScreen mode" >> main' FullScreen
        _               -> putStrLn $ "USAGE: " ++ prog ++ " [Windiwed|FullScreen]"


main' :: Display -> IO ()
main' dis = playIO dis
    black               -- Background color
    30                  -- Frames per second
    (initialState dis)  -- Initial state
    view                -- View function
    input               -- Event function
    step                -- Step function