{-# LANGUAGE OverloadedStrings #-}
module Day6 where

import           Data.Char (isSpace)

import qualified Data.Set  as S
import qualified Data.Text as T

day6_1 :: IO Int
day6_1 = do
    input <- T.splitOn "\n\n" . T.pack <$> readFile "src/input6.txt"
    pure $ sum $  length . S.fromList . T.unpack . T.filter (not . isSpace) <$> input

day6_2 :: IO Int
day6_2 = do
    input <- T.splitOn "\n\n" . T.pack <$> readFile "src/input6.txt"
    pure $ sum $ length . foldr1 S.intersection . map S.fromList . map T.unpack . T.lines <$> input
