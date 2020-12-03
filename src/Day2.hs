{-# LANGUAGE RecordWildCards #-}
module Day2 where

import qualified Data.Attoparsec.Text as A
import qualified Data.Text            as T

data Line = Line
    { min       :: !Int
    , max       :: !Int
    , character :: !Char
    , password  :: !T.Text
    } deriving Show

parseLine :: T.Text -> Either String Line
parseLine = A.parseOnly $ do
    min <- A.decimal
    A.char '-'
    max <- A.decimal
    A.skipSpace
    character <- A.anyChar
    A.take 2
    password <- A.takeText
    pure $ Line min max character password

validPassword :: Line -> Bool
validPassword Line {..} =
    let s = T.filter (character ==) password
    in T.length s >= min && T.length s <= max

day2_1 :: IO (Either String Int)
day2_1 = do
    input <- lines <$> readFile "input2.txt"
    let passwords = traverse (parseLine . T.pack) input
    pure $ length . filter validPassword <$> passwords

validPassword2 :: Line -> Bool
validPassword2 Line {..} =
    let c1 = T.index password (min - 1)
        c2 = T.index password (max - 1)
    in (c1 == character) /= (c2 == character)

day2_2 :: IO (Either String Int)
day2_2 = do
    input <- lines <$> readFile "input2.txt"
    let passwords = traverse (parseLine . T.pack) input
    pure $ length . filter validPassword2 <$> passwords
