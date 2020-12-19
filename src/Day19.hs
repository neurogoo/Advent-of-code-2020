{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Day19 where

import Control.Applicative

import qualified Data.Attoparsec.Text as A
import qualified Data.Map as Map
import qualified Data.Text as T

data Rule = Rule Int | Letter Char | Or [Rule] [Rule] deriving Show

parseFile :: T.Text -> Either String (Map.Map Int [Rule], [T.Text])
parseFile = A.parseOnly $ do
    rules <- Map.fromList <$> line `A.sepBy'` A.endOfLine
    A.endOfLine
    A.endOfLine
    ms <- A.takeTill A.isEndOfLine `A.sepBy'` A.endOfLine
    pure (rules,reverse $ drop 1 $ reverse ms)
  where
    line = do
        index <- A.decimal <* A.string ": "
        rules <- either <|> (rule `A.sepBy'` A.char ' ')
        pure (index, rules)
    either = do
        rules1 <- rule `A.sepBy'` A.char ' '
        A.skipSpace
        A.char '|'
        A.skipSpace
        rules2 <- rule `A.sepBy'` A.char ' '
        pure [Or rules1 rules2]
    rule = (Rule <$> A.decimal)
        <|> (Letter <$> (A.char '"' *> A.anyChar <* A.char '"'))

isValid :: Map.Map Int [Rule] -> String -> Bool
isValid rules = go (rules Map.! 0)
  where
    go [] [] = True
    go [] (_:_) = False
    go (_:_) [] = False
    go (Rule n:rs) s = go ((rules Map.! n) <> rs) s
    go (Letter c:rs) (s:ss) | c /= s = False
                            | otherwise = go rs ss
    go ((Or rs1 rs2):rs) s = go (rs1 <> rs) s || go (rs2 <> rs) s

day19_1 :: IO Int
day19_1 = do
    input <- readFile "src/input19.txt"
    let Right (rules, messages) = parseFile $ T.pack input
    pure $ length $ filter (isValid rules . T.unpack) messages

day19_2 :: IO Int
day19_2 = do
    input <- readFile "src/input19.txt"
    let Right (rules, messages) = parseFile $ T.pack input
    let rules' = Map.fromList [(8, [Or [Rule 42] [Rule 42, Rule 8]]), (11, [Or [Rule 42, Rule 31] [Rule 42, Rule 11, Rule 31]])] <> rules
    pure $ length $ filter (isValid rules' . T.unpack) messages
