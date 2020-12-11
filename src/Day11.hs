module Day11 where

import           Control.Monad (join)
import           Data.List     (find)
import           Data.Maybe    (catMaybes)

import qualified Data.Sequence as Seq

simulate :: Int -> Seq.Seq (Seq.Seq Char) -> Seq.Seq (Seq.Seq Char)
simulate rowLenght arr = fmap (\(y,arr) -> fmap (changeState y) arr) $ Seq.zip (Seq.fromList [0..rowLenght]) $ fmap (Seq.zip (Seq.fromList [0..rowLenght])) arr
  where
    neighbourSeats y x = catMaybes
        [ (y - 1) `Seq.lookup` arr >>= Seq.lookup x
        , (y - 1) `Seq.lookup` arr >>= Seq.lookup (x + 1)
        , y `Seq.lookup` arr >>= Seq.lookup (x + 1)
        , (y + 1) `Seq.lookup` arr >>= Seq.lookup (x + 1)
        , (y + 1) `Seq.lookup` arr >>= Seq.lookup x
        , (y + 1) `Seq.lookup` arr >>= Seq.lookup (x - 1)
        ,  y `Seq.lookup` arr >>= Seq.lookup (x - 1)
        , (y - 1) `Seq.lookup` arr >>= Seq.lookup (x - 1)
        ]
    changeState _ (_,'.') = '.'
    changeState y (x,'L') | all (/= '#') (neighbourSeats y x) = '#'
    changeState y (x,'#') | length (filter (== '#') $ neighbourSeats y x) >= 4 = 'L'
    changeState _ (_, c) = c

day11_1 :: IO Int
day11_1 = do
    input <- lines <$> readFile "src/input11.txt"
    let rowLength = length $ head input
    let findDifference previous =
            let new = simulate rowLength previous
            in if previous == new then
              length $ Seq.filter (== '#') $ join new
            else findDifference new
    pure $ findDifference $ Seq.fromList $ fmap Seq.fromList input

simulate2 :: Int -> Seq.Seq (Seq.Seq Char) -> Seq.Seq (Seq.Seq Char)
simulate2 rowLenght arr = fmap (\(y,arr) -> fmap (changeState y) arr) $ Seq.zip (Seq.fromList [0..rowLenght]) $ fmap (Seq.zip (Seq.fromList [0..rowLenght])) arr
  where
    seat (y,x) = y `Seq.lookup` arr >>= Seq.lookup x
    neighbourSeats y x = catMaybes
        [ join $ find (/= Just '.') (fmap seat (zip (reverse [0..y-1]) (repeat x)))
        , join $ find (/= Just '.') (fmap seat (zip (reverse [0..y-1]) [x+1..rowLenght]))
        , join $ find (/= Just '.') (fmap seat (zip (repeat y) [x+1..rowLenght]))
        , join $ find (/= Just '.') (fmap seat (zip [y+1..rowLenght] [x+1..rowLenght]))
        , join $ find (/= Just '.') (fmap seat (zip [y+1..rowLenght] (repeat x)))
        , join $ find (/= Just '.') (fmap seat (zip [y+1..rowLenght] (reverse [0..x-1])))
        , join $ find (/= Just '.') (fmap seat (zip (repeat y) (reverse [0..x-1])))
        , join $ find (/= Just '.') (fmap seat (zip (reverse [0..y-1]) (reverse [0..x-1])))
        ]
    changeState _ (_,'.') = '.'
    changeState y (x,'L') | all (/= '#') (neighbourSeats y x) = '#'
    changeState y (x,'#') | length (filter (== '#') $ neighbourSeats y x) >= 5 = 'L'
    changeState _ (_, c) = c

day11_2 :: IO Int
day11_2 = do
    input <- lines <$> readFile "src/input11.txt"
    let rowLength = length $ head input
    let findDifference previous =
            let new = simulate2 rowLength previous
            in if previous == new then
              length $ Seq.filter (== '#') $ join new
            else findDifference new
    pure $ findDifference $ Seq.fromList $ fmap Seq.fromList input
