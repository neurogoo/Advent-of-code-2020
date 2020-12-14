{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Day14 where

import           Control.Applicative
import           Data.Bits            (clearBit, setBit)
import           Data.Either          (rights)

import qualified Data.Attoparsec.Text as A
import qualified Data.Map             as Map
import qualified Data.Text            as T

maskBinary :: Int -> String -> Int
maskBinary = go 35
  where
    go :: Int -> Int -> String -> Int
    go _ val []         = val
    go bit val ('1':xs) = go (bit - 1) (val `setBit` bit) xs
    go bit val ('0':xs) = go (bit - 1) (val `clearBit` bit) xs
    go bit val (_:xs)   = go (bit - 1) val xs

toCommand :: T.Text -> Either String (Either String (Int,Int))
toCommand = A.parseOnly (mask <|> set)
  where
    mask = A.string "mask = " *> A.takeText >>= pure . Left . T.unpack
    set  = do
        place <- A.string "mem[" *> A.decimal
        number <- A.string "] = " *> A.decimal
        pure $ Right (place, number)

executeProgram :: [Either String (Int,Int)] -> [(Int, Int)]
executeProgram = go "" []
 where
   go _ acc []               = acc
   go _ acc ((Left mask):xs) = go mask acc xs
   go curMask acc ((Right (place, val)):xs) = go curMask (acc <> [(place, maskBinary val curMask)]) xs

day14_1 :: IO Int
day14_1 = do
    input <- lines <$> readFile "src/input14.txt"
    let commands = rights $ toCommand . T.pack <$> input
    pure $ sum $ Map.elems $ Map.fromList $ executeProgram commands

toAddresses :: Int -> String -> [Int]
toAddresses address = go 35 [address]
  where
    go :: Int -> [Int] -> String -> [Int]
    go _ acc [] = acc
    go bit acc ('X':xs) = go (bit - 1) (map (flip setBit bit) acc <> map (flip clearBit bit) acc) xs
    go bit acc ('1':xs) = go (bit - 1) (map (flip setBit bit) acc) xs
    go bit acc (_:xs) = go (bit - 1) acc xs

executeProgram2 :: [Either String (Int,Int)] -> [(Int, Int)]
executeProgram2 = go "" []
 where
   go _ acc []               = acc
   go _ acc ((Left mask):xs) = go mask acc xs
   go curMask acc ((Right (place, val)):xs) = go curMask (acc <> ((,val) <$> toAddresses place curMask) ) xs

day14_2 :: IO Int
day14_2 = do
    input <- lines <$> readFile "src/input14.txt"
    let commands = rights $ toCommand . T.pack <$> input
    pure $ sum $ Map.elems $ Map.fromList $ executeProgram2 commands
