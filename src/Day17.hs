module Day17 where

import qualified Data.Map as Map
import qualified Data.Set as Set

neighbours :: (Integer,Integer,Integer) -> [(Integer,Integer,Integer)]
neighbours (x,y,z) = [(x',y',z')
                     | x' <- [x-1..x+1]
                     , y' <- [y-1..y+1]
                     , z' <- [z-1..z+1]
                     , (x',y',z') /= (x,y,z)]

simulate :: (Ord a) => (a -> [a]) -> Map.Map a Bool -> Map.Map a Bool
simulate neighbours map' = Map.fromList $ foldMap go allNeighbours
  where
    neighbourStates  = map (\k -> Map.findWithDefault False k map') . neighbours
    allNeighbours = Set.fromList $ concatMap neighbours $ Map.keys map'
    go coord | Just True <- Map.lookup coord map' =
                       let states = length (filter (== True) (neighbourStates coord))
                       in if states == 2 || states == 3 then
                         [(coord, True)]
                       else
                         [(coord, False)]
    go coord = let states = length (filter (== True) (neighbourStates coord))
                   in if states == 3 then
                        [(coord, True)]
                      else
                        [(coord, False)]

day17_1 :: IO Int
day17_1 = do
    input <- lines <$> readFile "src/input17.txt"
    let numbered = zip [0..] $ fmap (zip [0..]) input
    let toCoord (y, xs) = fmap (\(x,char) -> if char == '#' then ((x,y,0),True) else ((x,y,0),False)) xs
    let res = Map.fromList $ concat $ toCoord <$> numbered
    pure $ length $ filter (== True) $ map snd $ Map.toList $ iterate (simulate neighbours) res !! 6

hyperNeighbours :: (Integer,Integer,Integer,Integer) -> [(Integer,Integer,Integer,Integer)]
hyperNeighbours (x,y,z,w) = [(x',y',z',w')
                            | x' <- [x-1..x+1]
                            , y' <- [y-1..y+1]
                            , z' <- [z-1..z+1]
                            , w' <- [w-1..w+1]
                            , (x',y',z',w') /= (x,y,z,w)]

day17_2 :: IO Int
day17_2 = do
    input <- lines <$> readFile "src/input17.txt"
    let numbered = zip [0..] $ fmap (zip [0..]) input
    let toCoord (y, xs) = fmap (\(x,char) -> if char == '#' then ((x,y,0,0),True) else ((x,y,0,0),False)) xs
    let res = Map.fromList $ concat $ toCoord <$> numbered
    pure $ length $ filter (== True) $ map snd $ Map.toList $ iterate (simulate hyperNeighbours) res !! 6
