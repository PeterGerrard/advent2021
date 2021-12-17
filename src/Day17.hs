module Day17 where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

parse :: String -> ((Integer, Integer), (Integer, Integer))
parse s = ((minx, maxx), (miny, maxy))
  where
    s1 = drop 15 s
    [(minx, '.' : '.' : s2)] = reads s1
    [(maxx, ',' : ' ' : 'y' : '=' : s3)] = reads s2
    [(miny, '.' : '.' : s4)] = reads s3
    maxy = read s4

print :: Integer -> String
print = show

gt :: Integer -> Maybe Integer -> Bool
gt x = maybe False (x >)

rangeMatch :: (Integer, Maybe Integer) -> (Integer, Maybe Integer) -> Bool
rangeMatch (min1, max1) (min2, max2) = not (gt min1 max2 || gt min2 max1)

solve :: ((Integer, Integer), (Integer, Integer)) -> Set (Integer, Integer)
solve ((minx, maxx), (miny, maxy)) = Set.fromList [(a, b) | a <- [0 .. maxx], b <- [miny .. (- miny)], l <- takeWhile (\(x, y) -> x <= maxx && y >= miny) . map (foldl (\(x1, y1) (x2, y2) -> (x1 + x2, y1 + y2)) (0, 0)) . inits $ iterate (\(x, y) -> (if x == 0 then 0 else x -1, y -1)) (a, b), inbounds l]
  where
    inbounds (x, y) = minx <= x && x <= maxx && miny <= y && y <= maxy

solve1 :: ((Integer, Integer), (Integer, Integer)) -> Integer
solve1 = (\n -> n * (n + 1) `div` 2) . maximum . Set.map snd . solve

solve2 :: ((Integer, Integer), (Integer, Integer)) -> Integer
solve2 = toInteger . Set.size . solve
