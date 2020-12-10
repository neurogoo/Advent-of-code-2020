module Day10 where

import           Data.List  (group, sort, subsequences, unfoldr)
import           Data.Maybe (listToMaybe, mapMaybe)
import           Text.Read  (readMaybe)

day10_1 :: IO (Maybe Int)
day10_1 = do
    input <- mapMaybe (readMaybe :: String -> Maybe Int) . lines <$> readFile "src/input10.txt"
    let buildInJoltage = maximum input + 3
    let chooseAdapter :: (Int, [Int]) -> Maybe (Int,(Int, [Int]))
        chooseAdapter (_, [])      = Nothing
        chooseAdapter (curjol,adapters) =
            let adapter = minimum $ filter (\a -> a - curjol <= 3) adapters
                adapters' = filter (/= adapter) adapters
            in Just (adapter - curjol, (adapter, adapters'))
    let res = map (\x -> (head x,length x)) $ group $ sort $ unfoldr chooseAdapter (0, input <> [buildInJoltage])
    pure $ (*) <$> 3 `lookup` res <*>  1 `lookup` res

day10_2 :: IO Int
day10_2 = do
    input <- mapMaybe (readMaybe :: String -> Maybe Int) . lines <$> readFile "src/input10.txt"
    let buildInJoltage = maximum input + 3
    let adapterCombinations :: Int -> [Int] -> Int -> Int
        adapterCombinations _ [] acc = acc
        adapterCombinations curJol adapters acc =
            let subseq = takeWhile (\a -> a - curJol <= 3) adapters
                rest   = dropWhile (\a -> a - curJol <= 3) adapters
                nextStart = listToMaybe rest
            in case (subseq, nextStart) of
              ([], _) -> acc
              (_, Nothing) -> acc
              ([x], Just _) -> adapterCombinations x rest acc
              (as, Just val) ->
                  let validSubsequences = length $ filter (\xs -> val - last xs <= 3 ) $ filter (/= []) $ subsequences as
                  in adapterCombinations (last as) rest (acc * validSubsequences)
    pure $ adapterCombinations 0 (sort $ input <> [buildInJoltage]) 1
