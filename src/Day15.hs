module Day15 where

import qualified Data.IntMap as Map

toMap :: Int -> [Int] -> Int
toMap endTurn arr = intmap (last arr) (length arr + 1) (Map.fromList $ zip arr [1..])
  where
    intmap prevNumber turn _ | turn == endTurn = prevNumber
    intmap prevNumber turn map | Nothing <- map Map.!? prevNumber =
                                     intmap 0 (turn + 1) (Map.insert prevNumber (turn-1) map)
    intmap prevNumber turn map | Just y <- map Map.!? prevNumber =
                                     intmap (turn - 1 - y) (turn + 1) (Map.insert prevNumber (turn - 1) map)

day15_1 :: Int
day15_1 = toMap 2021 [1,0,16,5,17,4]

day15_2 :: Int
day15_2 = toMap 30000001 [1,0,16,5,17,4]
