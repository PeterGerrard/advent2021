module Day14 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

type Element = Char

alter :: Ord k => k -> a -> (a -> a -> a) -> Map k a -> Map k a
alter k v f = Map.alter (Just . maybe v (f v)) k

applyPolymerization :: ((Element, Element) -> Element) -> Map (Element, Element) Integer -> Map (Element, Element) Integer
applyPolymerization poly = Map.foldlWithKey applyPoly Map.empty
  where
    applyPoly :: Map (Element, Element) Integer -> (Element, Element) -> Integer -> Map (Element, Element) Integer
    applyPoly m (e1, e2) v = alter (e1, e3) v (+) $ alter (e3, e2) v (+) m
      where
        e3 = poly (e1, e2)

parseTemplate :: String -> ((Element, Element), Element)
parseTemplate [e1, e2, _, _, _, _, e3] = ((e1, e2), e3)

parse :: String -> ((Element, Element) -> Element, Map (Element, Element) Integer, Element, Element)
parse s = (fromJust . (`Map.lookup` m), Map.fromListWith (+) $ zipWith (\a b -> ((a, b), 1)) c (tail c), head c, last c)
  where
    (c : "" : ts) = lines s
    m = Map.fromList $ map parseTemplate ts

print :: Integer -> String
print = show

solve :: Int -> ((Element, Element) -> Element, Map (Element, Element) Integer, Element, Element) -> Integer
solve n (p, ps, f, l) = uncurry (-) . maxMin . Map.elems . countElements . (!! n) $ iterate (applyPolymerization p) ps
  where
    maxMin xs = (maximum xs, minimum xs)
    countElements :: Map (Element, Element) Integer -> Map Element Integer
    countElements = Map.update (Just . (1 +)) l . Map.update (Just . (1 +)) f . Map.map (`div` 2) . Map.fromListWith (+) . concatMap (\((e1, e2), v) -> [(e1, v), (e2, v)]) . Map.toList

solve1 = solve 10

solve2 = solve 40
