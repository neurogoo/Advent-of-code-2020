module Day5 where

import           Data.Maybe (listToMaybe, mapMaybe)

import qualified Data.Set   as S

uncompress :: String -> Maybe (Int,Int)
uncompress line = go line ([0..127], [0..7])
  where
    go [] (rows,columns)          = (,) <$> listToMaybe rows <*> listToMaybe columns
    go ('F' : ls) (rows, columns) = go ls (take (length rows `div` 2) rows,columns)
    go ('B' : ls) (rows, columns) = go ls (drop (length rows `div` 2) rows,columns)
    go ('R' : ls) (rows, columns) = go ls (rows,drop (length columns `div` 2) columns)
    go ('L' : ls) (rows, columns) = go ls (rows,take (length columns `div` 2) columns)
    go (_ : ls) (rows, columns)   = go ls (rows,columns)

day5_1 :: IO Int
day5_1 = do
    input <- lines <$> readFile "src/input5.txt"
    pure $ maximum $ map (\(row,column) -> row * 8 + column) $ mapMaybe uncompress input

day5_2 :: IO Int
day5_2 = do
    input <- lines <$> readFile "src/input5.txt"
    let allSeats = [0 .. (127*8 + 7)]
    let scannedSeats = S.fromList $ map (\(row,column) -> row * 8 + column) $ mapMaybe uncompress input
    let freeSeats = filter (`S.notMember` scannedSeats) allSeats
    pure $ snd $ head $ filter (\(cur,next) -> next - cur > 1) $ zip freeSeats (drop 1 freeSeats)
