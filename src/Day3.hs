module Day3 where

day3_1 :: IO Int
day3_1 = do
    input <- fmap cycle . lines <$> readFile "input3.txt"
    let yMax = length input
    let route = takeWhile (\(_,y) -> y < yMax) $ iterate (\(x,y) -> (x+3,y+1)) (0,0)
    let hasTree (x,y) = (input !! y) !! x == '#'
    pure $ length $ filter hasTree route

day3_2 :: IO Int
day3_2 = do
    input <- fmap cycle . lines <$> readFile "input3.txt"
    let yMax = length input
    let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    let route (addX,addY) = takeWhile (\(_,y) -> y < yMax) $ iterate (\(x,y) -> (x+addX,y+addY)) (0,0)
    let routes = route <$> slopes
    let hasTree (x,y) = (input !! y) !! x == '#'
    pure $ product $ length . filter hasTree <$> routes
