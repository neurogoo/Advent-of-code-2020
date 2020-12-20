{-# LANGUAGE OverloadedStrings #-}
module Day20 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (transpose, delete, permutations, isInfixOf)

import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map as Map

type Tile = [String]

toTile :: T.Text -> Maybe (Int, Tile)
toTile t =
    let (index:tile) = T.splitOn "\n" t
        dropLast = reverse . drop 1 . reverse
        index' = readMaybe $ dropLast $ drop 5 $ T.unpack index
    in (,) <$> index' <*> Just (fmap T.unpack tile)

findPairs :: [(Int, Tile)] -> [(Int, [Int])]
findPairs tiles = fmap (\(tid, tile) -> (tid , fmap fst (filter (commonSide tile . snd) (delete (tid, tile) tiles)))) tiles
  where
    commonSide tile1 tile2 = not (null (Set.intersection (allSides tile1) (allSides tile2)))
    allSides tile = Set.fromList (tilesides tile <> tilesides (flipx tile) <> tilesides (flipy tile))
    flipx tile = fmap reverse tile
    flipy tile = reverse tile
    tilesides tile =
        [ head tile
        , head $ transpose tile
        , last $ transpose tile
        , last tile
        ]

day20_1 :: IO Int
day20_1 = do
    input <- T.pack <$> readFile "src/input20.txt"
    let tiles = mapMaybe toTile $ T.splitOn "\n\n" input
    let test tiles = filter (\(_,neighbours) -> length neighbours == 2) $ findPairs tiles
    let mmap = Map.fromList $ findPairs tiles
    pure $ product $ map fst $ filter (\(_,n) -> all (\n' -> (length <$> Map.lookup n' mmap) == Just 3) n) $ test tiles
