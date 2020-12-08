module Day8 where

import           Text.Read     (readMaybe)

import qualified Data.Sequence as Seq
import qualified Data.Set      as S


run :: Seq.Seq [String] -> Either Int Int
run instr = go 0 0 mempty
  where
    toNumber = readMaybe . filter (/= '+')
    go next acc _       | next >= length instr    = Left acc
    go next acc visited | next `S.member` visited = Right acc
    go next acc visited =
        let visited' = next `S.insert` visited
        in case next `Seq.lookup` instr of
          Just ("acc":val:_) | Just val' <- toNumber val -> go (next + 1) (acc + val') visited'
          Just ("jmp":val:_) | Just val' <- toNumber val -> go (next + val') acc visited'
          _                                              -> go (next + 1) acc visited'

day8_1 :: IO (Either Int Int)
day8_1 = do
    input <- lines <$> readFile "src/input8.txt"
    let input' = Seq.fromList $ words <$> input
    pure $ run input'

day8_2 :: IO (Either Int [Int])
day8_2 = do
    input <- lines <$> readFile "src/input8.txt"
    let input' = Seq.fromList $ words <$> input
    let modifyProgram index instr | Just ("nop":xs) <- index `Seq.lookup` instr = Seq.update index ("jmp":xs) instr
                                  | Just ("jmp":xs) <- index `Seq.lookup` instr = Seq.update index ("nop":xs) instr
        modifyProgram _ instr = instr
    let modifiedPrograms = [modifyProgram index input' | index <- [0..(length input')]]
    pure $ traverse run modifiedPrograms
