module Day11 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

parse :: String -> Map (Integer, Integer) Integer
parse = Map.map (read . (: [])) . toMap . lines

print :: Integer -> String
print = show

toMap :: [[a]] -> Map (Integer, Integer) a
toMap = Map.fromList . concatMap (\(y, row) -> zipWith (\x c -> ((x, y), c)) [0 ..] row) . zip [0 ..]

applyDay :: Map (Integer, Integer) Integer -> Map (Integer, Integer) Integer
applyDay m = applyFlashes $ Map.map (1 +) m
  where
    applyFlashes :: Map (Integer, Integer) Integer -> Map (Integer, Integer) Integer
    applyFlashes m = if Map.size (Map.filter (> 9) m) == 0 then m else applyFlashes (foldl (\ms a -> if maybe False (> 9) (Map.lookup a m) then flash ms a else ms) m positions)
    flash :: Map (Integer, Integer) Integer -> (Integer, Integer) -> Map (Integer, Integer) Integer
    flash m p = foldl (flip (Map.alter (fmap (\x -> if x == 0 then 0 else x + 1)))) (Map.alter (fmap (const 0)) p m) $ adjacent p
    adjacent :: (Num a, Eq a) => (a, a) -> [(a, a)]
    adjacent (x, y) = [(x + a, y + b) | a <- [-1, 0, 1], b <- [-1, 0, 1], a /= 0 || b /= 0]
    positions :: [(Integer, Integer)]
    positions = [(x, y) | x <- [minx .. maxx], y <- [miny .. maxy]]
    cs = Map.keys m
    minx = minimum $ map fst cs
    maxx = maximum $ map fst cs
    miny = minimum $ map snd cs
    maxy = maximum $ map snd cs

solve1 :: Map (Integer, Integer) Integer -> Integer
solve1 = sum . map (toInteger . Map.size . Map.filter (== 0)) . take 100 . drop 1 . iterate applyDay

solve2 :: Map (Integer, Integer) Integer -> Integer
solve2 = toInteger . length . takeWhile (== True) . map (any (/= 0) . Map.elems) . iterate applyDay
