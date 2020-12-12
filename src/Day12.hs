module Day12 where

import           Data.List  (foldl')
import           Data.Maybe (mapMaybe)
import           Text.Read  (readMaybe)

toCommand :: String -> Maybe (Char,Int)
toCommand (x:xs) | Just val <- readMaybe xs = Just (x,val)
toCommand _ = Nothing

step :: (Char,(Int,Int)) -> (Char,Int) -> (Char,(Int,Int))
step (facing,(x,y)) ('N',val) = (facing, (x, y - val))
step (facing,(x,y)) ('S',val) = (facing, (x, y + val))
step (facing,(x,y)) ('E',val) = (facing, (x - val, y))
step (facing,(x,y)) ('W',val) = (facing, (x + val, y))
step (facing,(x,y)) ('L',val) =
    let nextDir = head $ drop (val `div` 90) $ dropWhile (/= facing) $ cycle ['N','W','S','E']
    in (nextDir, (x, y))
step (facing,(x,y)) ('R',val) =
    let nextDir = head $ drop (val `div` 90) $ dropWhile (/= facing) $ cycle ['N','E','S','W']
    in (nextDir, (x, y))
step (facing,(x,y)) ('F',val) = step (facing, (x, y)) (facing, val)
step xs _                     = xs

day12_1 :: IO Int
day12_1 = do
    input <- mapMaybe toCommand . lines <$> readFile "src/input12.txt"
    let (x,y) = snd $ foldl' step ('E',(0,0)) input
    pure $ abs x + abs y

step2 :: ((Int,Int),(Int,Int)) -> (Char,Int) -> ((Int,Int),(Int,Int))
step2 ((xway,yway),(x,y)) ('N',val) = ((xway,yway - val), (x, y))
step2 ((xway,yway),(x,y)) ('S',val) = ((xway,yway + val), (x, y))
step2 ((xway,yway),(x,y)) ('E',val) = ((xway + val,yway), (x, y))
step2 ((xway,yway),(x,y)) ('W',val) = ((xway - val,yway), (x, y))
step2 ((xway,yway),(x,y)) ('L',val)  =
    let radians = (*) (pi/180.0) . fromIntegral
        val' = -val
        newX = round (cos (radians val') * fromIntegral xway) - round (sin (radians val') * fromIntegral yway)
        newY = round (sin (radians val') * fromIntegral xway) + round (cos (radians val') * fromIntegral yway)
    in ((newX,newY), (x, y))
step2 ((xway,yway),(x,y)) ('R',val) =
    let radians = (*) (pi/180.0) . fromIntegral
        val' = val
        newX = round (cos (radians val') * fromIntegral xway) - round (sin (radians val') * fromIntegral yway)
        newY = round (sin (radians val') * fromIntegral xway) + round (cos (radians val') * fromIntegral yway)
    in ((newX,newY), (x, y))
step2 ((xway,yway),(x,y)) ('F',val) = ((xway,yway), (x + val * xway, y + val * yway))
step2 xs _                                 = xs


day12_2 :: IO Int
day12_2 = do
    input <- mapMaybe toCommand . lines <$> readFile "src/input12.txt"
    let (x,y) = snd $ foldl' step2 ((10,-1),(0,0)) input
    pure $ abs x + abs y
