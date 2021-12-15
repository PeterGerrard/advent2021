module Day15 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data Graph a = Graph
  { nodes :: [a],
    edges :: Map a [(a, Integer)],
    nodeCost :: Map a Integer
  }

dijkstra :: (Eq a, Ord a, Show a) => Graph a -> a -> a -> Integer
dijkstra g s e = go (Set.singleton (0, s)) Set.empty
  where
    go toVisit visited = case Set.deleteFindMin toVisit of
      (v@(cost, n), rest)
        | n == e -> cost
        | n `Set.member` visited -> go rest visited
        | otherwise -> go (Set.union rest $ adjacent v) (Set.insert n visited)
        where
          adjacent (cost, e) = Set.fromList . map (\(e2, c) -> (c + cost, e2)) . filter (\(e2, c) -> not $ Set.member e2 visited) $ fromMaybe [] (Map.lookup e es)
    es = edges g

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

print :: Integer -> String
print = show

solve1 :: Graph (Integer, Integer) -> Integer
solve1 g = dijkstra g (0, 0) (maximum $ map fst ns, maximum $ map snd ns)
  where
    ns = nodes g

wrap :: Integer -> Integer
wrap n = if n < 10 then n else wrap (n - 9)

tile :: Integer -> Graph (Integer, Integer) -> Graph (Integer, Integer)
tile t g = Graph {nodes = ns, edges = Map.fromList [(x, [(a, c) | a <- adjacent x, Just c <- [Map.lookup a nodeCosts]]) | x <- ns], nodeCost = nodeCosts}
  where
    ns = Map.keys nodeCosts
    nodeCosts = Map.fromList $ concatMap (\(xo, yo) -> map (\((x, y), c) -> ((x + (mx + 1) * xo, y + (my + 1) * yo), wrap (c + xo + yo))) $ Map.toList $ nodeCost g) [(xo, yo) | xo <- [0 .. t -1], yo <- [0 .. t -1]]
    my = maximum . map snd $ nodes g
    mx = maximum . map fst $ nodes g

solve2 :: Graph (Integer, Integer) -> Integer
solve2 = solve1 . tile 5
