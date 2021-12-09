module Day09 where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> Map (Integer, Integer) Integer
parse = Map.fromList . concat . zipWith (\x xs -> map (\(y, n) -> ((x, y), read [n])) xs) [0 ..] . map (zip [0 ..]) . lines

adjacent :: (Integer, Integer) -> [(Integer, Integer)]
adjacent (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

isLowerThanAdjacent :: Map (Integer, Integer) Integer -> (Integer, Integer) -> Bool
isLowerThanAdjacent m p = all (maybe True (> h)) $ map (`Map.lookup` m) $ adjacent p
  where
    h = fromJust (Map.lookup p m)

getLowPoints :: Map (Integer, Integer) Integer -> [(Integer, Integer)]
getLowPoints m = filter (isLowerThanAdjacent m) $ Map.keys m

print :: Integer -> String
print = show

solve1 :: Map (Integer, Integer) Integer -> Integer
solve1 m = sum $ map (maybe 0 (1 +) . (`Map.lookup` m)) (getLowPoints m)

getBasin :: Map (Integer, Integer) Integer -> (Integer, Integer) -> [(Integer, Integer)]
getBasin m p = Set.toList $ go (Set.fromList [p])
  where
    go :: Set (Integer, Integer) -> Set (Integer, Integer)
    go s = if Set.size s == Set.size ns then s else go ns
      where
        ns = Set.filter (maybe False (< 9) . (`Map.lookup` m)) $ Set.union s $ Set.unions $ Set.map (Set.fromList . adjacent) s

solve2 :: Map (Integer, Integer) Integer -> Integer
solve2 m = product $ take 3 $ sortBy (flip compare) $ map (toInteger . length . getBasin m) $ getLowPoints m