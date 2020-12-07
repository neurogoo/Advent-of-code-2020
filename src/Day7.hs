{-# LANGUAGE OverloadedStrings #-}
module Day7 where

import           Control.Applicative
import           Data.Char

import qualified Data.Attoparsec.Text as A
import qualified Data.Map             as M
import qualified Data.Text            as T

parseRequirement :: T.Text -> Either String (T.Text, [(Int, T.Text)])
parseRequirement = A.parseOnly $ do
    color <- A.manyTill A.anyChar (A.string " bags contain ")
    let noBags = do
            A.string "no other bags."
            pure []
    let bags = A.many1 $ do
            n <- A.decimal
            A.skipSpace
            color <- A.manyTill A.anyChar (A.string " bag")
            A.skipWhile (not . isSpace)
            A.skipSpace
            pure (n, T.pack color)
    rest <- noBags <|> bags
    pure (T.pack color, rest)

findPossibilities :: T.Text -> [(T.Text, [(Int, T.Text)])] -> Int
findPossibilities color bags = length $ filter id $ go . fst <$> bags
  where
    bagMap = M.fromList bags
    go c | c == color = True
    go curColor =
        case curColor `M.lookup` bagMap of
          Just [] -> False
          Just xs -> any go $ fmap snd xs
          Nothing -> False

day7_1 :: IO (Either String Int)
day7_1 = do
    input <- lines <$> readFile "src/input7.txt"
    let bags = traverse (parseRequirement . T.pack) input
    pure $ findPossibilities "shiny gold" . filter (\(color, _) -> color /= "shiny gold") <$> bags

bagAmount :: T.Text -> [(T.Text, [(Int, T.Text)])] -> Int
bagAmount color bags = maybe 0 go (M.lookup color bagMap)
    where
      bagMap = M.fromList bags
      go [] = 0
      go xs = sum $ (\(n, x) -> maybe 0 (\bs -> n + (n * go bs)) $ x `M.lookup` bagMap) <$> xs

day7_2 :: IO (Either String Int)
day7_2 = do
    input <- lines <$> readFile "src/input7.txt"
    let x = traverse (parseRequirement . T.pack) input
    pure $ bagAmount "shiny gold" <$> x
