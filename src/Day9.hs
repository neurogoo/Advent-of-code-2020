module Day9 where

import           Data.Maybe (listToMaybe, mapMaybe)
import           Text.Read  (readMaybe)

import qualified Data.Set   as S

searchInvalid :: Int -> [Int] -> Int
searchInvalid window arr =
    let compArr = take window arr
        val     = arr !! window
    in if val `S.member` S.fromList [ x + y | x <- compArr, y <- compArr, x /= y] then
         searchInvalid window (drop 1 arr)
       else
         val

day9_1 :: IO Int
day9_1 = do
    input <- mapMaybe (readMaybe :: String -> Maybe Int) . lines <$> readFile "src/input9.txt"
    pure $ searchInvalid 25 input

day9_2 :: IO (Maybe Int)
day9_2 = do
    input <- mapMaybe (readMaybe :: String -> Maybe Int) . lines <$> readFile "src/input9.txt"
    let invalidNumber = searchInvalid 25 input
    let searchSequence :: [Int] -> Int -> Maybe [Int]
        searchSequence arr len | length arr < len = Nothing
                               | sum (take len arr) == invalidNumber = Just $ take len arr
                               | otherwise = searchSequence (drop 1 arr) len
    let res = listToMaybe $ mapMaybe (searchSequence input) [2..(length input - 1)]
    pure $ (\r -> minimum r + maximum r) <$> res
