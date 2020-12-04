{-# LANGUAGE OverloadedStrings #-}
module Day4 where

import           Control.Applicative
import           Data.Char            (isDigit, isSpace)
import           Text.Read            (readMaybe)

import qualified Data.Attoparsec.Text as A
import qualified Data.Set             as S
import qualified Data.Text            as T

parseEntry :: A.Parser (T.Text, T.Text)
parseEntry = do
    key <- A.take 3
    A.char ':'
    value <- A.takeWhile1 (not . isSpace)
    pure (key,value)

parsePassport :: T.Text -> Either String [[(T.Text,T.Text)]]
parsePassport = A.parseOnly $ do
    A.many1 $ do
        passport <- A.many1 (parseEntry <* A.space)
        A.skipSpace <|> A.endOfInput
        pure passport

day4_1 :: IO (Either String Int)
day4_1 = do
    input <- readFile "src/input4.txt"
    let passports = parsePassport $ T.pack input
    let keycheck xs = S.fromList (filter (/= "cid") $ fst <$> xs) == S.fromList ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
    pure $ length . filter keycheck <$> passports

parseHgt :: T.Text -> Maybe Bool
parseHgt = A.maybeResult . A.parse
       ((do
              n <- A.decimal <* A.string "cm"
              pure $ n >= 150 && n <= 193)
        <|>
        (do
              n <- A.decimal <* A.string "in"
              pure $ n >= 59 && n <= 76))

parseHcl :: T.Text -> Maybe T.Text
parseHcl = A.maybeResult . A.parse (do
    _ <- A.char '#'
    A.take 6)

fieldValid :: (T.Text, T.Text) -> Bool
fieldValid ("byr", val) | Just val' <- readMaybe (T.unpack val) = val' >= 1920 && val' <= 2002
fieldValid ("iyr", val) | Just val' <- readMaybe (T.unpack val) = val' >= 2010 && val' <= 2020
fieldValid ("eyr", val) | Just val' <- readMaybe (T.unpack val) = val' >= 2020 && val' <= 2030
fieldValid ("hgt", val) | Just val' <- parseHgt val = val'
fieldValid ("hcl", val) | Just val' <- parseHcl val = all (A.inClass "0-9a-f") $ T.unpack val'
fieldValid ("ecl", val) = val `S.member` S.fromList ["amb","blu","brn","gry","grn","hzl","oth"]
fieldValid ("pid", val) | T.length val == 9 = all isDigit $ T.unpack val
fieldValid _            = False


day4_2 :: IO (Either String Int)
day4_2 = do
    input <- readFile "src/input4.txt"
    let passports = parsePassport $ T.pack input
    let keycheck xs = S.fromList (fst <$> xs) == S.fromList ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
    let fieldCheck = all fieldValid
    pure $ length . filter fieldCheck . filter keycheck . map (filter (\(k,_) -> k /= "cid")) <$> passports
