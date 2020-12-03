module Day1 where

day1_1 :: IO Int
day1_1 = do
    input <- lines <$> readFile "input1.txt"
    let numbers = (read :: String -> Int) <$> input
    pure $ uncurry (*) $ head $ filter (\(n1,n2) -> n1 + n2 == 2020) $ [(n1,n2) | n1 <- numbers, n2 <- numbers, n1 /= n2]

day1_2 :: IO Int
day1_2 = do
    input <- lines <$> readFile "input1.txt"
    let numbers = (read :: String -> Int) <$> input
    pure $ (\(n1,n2,n3) -> n1 * n2 * n3) $ head $ filter (\(n1,n2,n3) -> n1 + n2 + n3 == 2020) $ [(n1,n2,n3) | n1 <- numbers, n2 <- numbers, n3 <- numbers, n1 /= n2, n2 /= n3, n1 /= n3]
