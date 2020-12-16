{-# LANGUAGE OverloadedStrings #-}
module Day16 where

import           Control.Applicative
import           Data.List            (delete, transpose)
import           Data.Maybe           (mapMaybe)
import           Text.Read            (readMaybe)

import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T

data Info = Info
    { rules         :: [(T.Text, ((Int,Int),(Int,Int)))]
    , myTicket      :: [Int]
    , nearbyTickets :: [[Int]]
    } deriving Show

parseData :: T.Text -> Either String Info
parseData = A.parseOnly $ Info
    <$> (rules <* A.skipSpace)
    <*> myTicket
    <*> nearbyTickets
  where
    rules = many $ do
        rule <- A.takeTill (== ':') <* A.string ": "
        lower1 <- A.decimal <* A.string "-"
        lower2 <- A.decimal <* A.string " or "
        upper1 <- A.decimal <* A.string "-"
        upper2 <- A.decimal <* A.skipSpace
        pure (rule, ((lower1,lower2), (upper1,upper2)))
    ticketNumbers = do
        line <- A.takeTill A.isEndOfLine
        A.skipSpace
        pure $ mapMaybe (readMaybe . T.unpack) $ T.splitOn "," line
    myTicket = A.string "your ticket:" *> A.endOfLine *> ticketNumbers
    nearbyTickets = do
        A.string "nearby tickets:"
        A.endOfLine
        A.manyTill ticketNumbers A.endOfInput

compareToBounds :: Int -> ((Int,Int),(Int,Int)) -> Bool
compareToBounds x ((a1,a2),(b1,b2)) = (x >= a1 && x <= a2) || (x >= b1 && x <= b2)

day16_1 :: IO Int
day16_1 = do
    input <- T.pack <$> readFile "src/input16.txt"
    let Right(Info rules _ tickets) = parseData input
    let comparisons ts = [t | t <- ts, not (any (compareToBounds t . snd) rules)]
    pure $ sum $ concat $ comparisons <$> tickets

findOrder :: [[Int]] -> [(T.Text, ((Int,Int),(Int,Int)))] -> [Int]
findOrder arr rules = go [] rules (zip [0..] $ transpose arr)
  where
    go acc _ [] = acc
    go acc rules (ts@(tindex,t):tss) =
        case filter (\(_,bounds) -> all (flip compareToBounds bounds) t) rules of
          [rule@(name,_)] ->
              if T.isPrefixOf "departure" name then
                go (acc <> [tindex]) (delete rule rules) tss
              else
                go acc (delete rule rules) tss
          _:_                     -> go acc rules (tss <> [ts])
          []                      -> error "No match found"

day16_2 :: IO Int
day16_2 = do
    input <- T.pack <$> readFile "src/input16.txt"
    let Right(Info rules myticket tickets) = parseData input
    let comparisons ts = any (\t -> not (any (compareToBounds t . snd) rules)) ts
    let validTickets = filter (not . comparisons) tickets
    let order = findOrder validTickets rules
    pure $ product $ (myticket !!) <$> order
