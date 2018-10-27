module Level where

import Data.List
import Variables

-- getfield takes 1 char and makes it a field
getField :: Char -> Field
getField 'W' = WallField
getField 'P' = PowerField
getField 'E' = EmptyField
getField 'C' = CoinField
getField 'A' = PacSpawnField
getField 'B' = BonusField
getField 'S' = EnemySpawnField


loadLevelFromFile :: IO Board
loadLevelFromFile = do 
                    _string <- readFile "levels/level1.txt"
                    let splitString = lines _string
                    let rows = map getRows splitString
                    return $ rows
                        where getRows x = map getField x
