module Day15 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.SortedList (SortedList, toSortedList)
import qualified Data.SortedList as SortedList

data ToVisitNodes a = ToVisit a [a] Integer

instance Eq (ToVisitNodes a) where
  (==) (ToVisit _ _ a) (ToVisit _ _ b) = a == b

instance Ord (ToVisitNodes a) where
  compare (ToVisit _ _ x) (ToVisit _ _ y) = compare x y

data Graph a = Graph
  { nodes :: [a],
    edges :: Map a [(a, Integer)],
    nodeCost :: Map a Integer
  }

dijkstra :: (Eq a, Ord a) => Graph a -> a -> a -> ([a], Integer)
dijkstra g s e = go (toSortedList [ToVisit s [s] 0]) Set.empty
  where
    go toVisit visited = case SortedList.uncons toVisit of
      Nothing -> error "Cannot reach end"
      Just (v@(ToVisit n path cost), rest)
        | n == e -> (path, cost)
        | n `Set.member` visited -> go rest visited
        | otherwise -> go (addAdjacent v rest) (Set.insert n visited)
    addAdjacent (ToVisit e path cost) l = foldl (flip SortedList.insert) l $ map (\(e2, c) -> ToVisit e2 (e2 : path) (c + cost)) $ fromMaybe [] (Map.lookup e (edges g))

adjacent :: (Integer, Integer) -> [(Integer, Integer)]
adjacent (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]

lookupWithDefault :: Ord k => a -> k -> Map k a -> a
lookupWithDefault x k = fromMaybe x . Map.lookup k

parse :: String -> Graph (Integer, Integer)
parse s = Graph {nodes = nodes, edges = Map.fromList [(x, [(a, c) | a <- adjacent x, Just c <- [Map.lookup a nodeCosts]]) | x <- nodes], nodeCost = nodeCosts}
  where
    nodeCosts :: Map (Integer, Integer) Integer
    nodeCosts = Map.fromList . concatMap (\(y, row) -> zipWith (\x c -> ((x, y), read [c])) [0 ..] row) . zip [0 ..] $ lines s
    nodes = Map.keys nodeCosts

print :: ([(Integer, Integer)], Integer) -> String
print = show . snd

solve1 :: Graph (Integer, Integer) -> ([(Integer, Integer)], Integer)
solve1 g = dijkstra g (0, 0) (maximum $ map fst ns, maximum $ map snd ns)
  where
    ns = nodes g

wrap :: Integer -> Integer
wrap n = if n < 10 then n else wrap (n -9)

tileX :: Integer -> Graph (Integer, Integer) -> Graph (Integer, Integer)
tileX t g = Graph {nodes = ns, edges = Map.fromList [(x, [(a, c) | a <- adjacent x, Just c <- [Map.lookup a nodeCosts]]) | x <- ns], nodeCost = nodeCosts}
  where
    ns = Map.keys nodeCosts
    nodeCosts = Map.fromList $ concatMap (\n -> map (\((x, y), c) -> ((x + (mx + 1) * n, y), wrap (c + n))) $ Map.toList $ nodeCost g) [0 .. t -1]
    mx = maximum . map fst $ nodes g

tileY :: Integer -> Graph (Integer, Integer) -> Graph (Integer, Integer)
tileY t g = Graph {nodes = ns, edges = Map.fromList [(x, [(a, c) | a <- adjacent x, Just c <- [Map.lookup a nodeCosts]]) | x <- ns], nodeCost = nodeCosts}
  where
    ns = Map.keys nodeCosts
    nodeCosts = Map.fromList $ concatMap (\n -> map (\((x, y), c) -> ((x, y + (my + 1) * n), wrap (c + n))) $ Map.toList $ nodeCost g) [0 .. t -1]
    my = maximum . map snd $ nodes g

solve2 :: Graph (Integer, Integer) -> ([(Integer, Integer)], Integer)
solve2 = solve1 . tileY 5 . tileX 5
