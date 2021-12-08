module Day08 where

import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type Segment = Char

type DigitDisplay = [Segment]

type Input = [([DigitDisplay], [DigitDisplay])]

type Output = [[Integer]]

getDigitDisplay :: Integer -> DigitDisplay
getDigitDisplay x = case x of
  0 -> ['a', 'b', 'c', 'e', 'f', 'g']
  1 -> ['c', 'f']
  2 -> ['a', 'c', 'd', 'e', 'g']
  3 -> ['a', 'c', 'd', 'f', 'g']
  4 -> ['b', 'c', 'd', 'f']
  5 -> ['a', 'b', 'd', 'f', 'g']
  6 -> ['a', 'b', 'd', 'e', 'f', 'g']
  7 -> ['a', 'c', 'f']
  8 -> ['a', 'b', 'c', 'd', 'e', 'f', 'g']
  9 -> ['a', 'b', 'c', 'd', 'f', 'g']
  _ -> error "Must be a digit"

allDisplays :: [DigitDisplay]
allDisplays = map getDigitDisplay [0 .. 9]

getMatching :: DigitDisplay -> Map Segment Segment -> [DigitDisplay]
getMatching d m = filter (\x -> length x == length d && all (\s -> maybe True (`elem` x) (Map.lookup s m)) d) allDisplays

parseLine :: String -> ([DigitDisplay], [DigitDisplay])
parseLine s = (i, d)
  where
    [i, d] = map (map sort . words) $ splitOn " | " s

parse :: String -> Input
parse = map parseLine . lines

print1 :: Output -> String
print1 = show . sum . map length

solve1 :: Input -> Output
solve1 =
  map
    ( ( map
          ( \x -> case length x of
              2 -> 1
              3 -> 7
              4 -> 4
              7 -> 8
              _ -> error "Woops"
          )
          . filter
            ( \x -> case length x of
                2 -> True
                3 -> True
                4 -> True
                7 -> True
                _ -> False
            )
      )
        . snd
    )

findMapping :: [DigitDisplay] -> Map DigitDisplay Integer
findMapping xs = Map.fromList $ zip [x0, x1, x2, x3, x4, x5, x6, x7, x8, x9] [0 ..]
  where
    x1 = head $ filter ((== 2) . length) xs
    x4 = head $ filter ((== 4) . length) xs
    x7 = head $ filter ((== 3) . length) xs
    x8 = head $ filter ((== 7) . length) xs
    x235 = filter ((== 5) . length) xs
    x069 = filter ((== 6) . length) xs
    ([x6], x09) = partition ((== 1) . length . (x1 \\)) x069
    ([x3], x25) = partition (null . (x1 \\)) x235
    ([x9], [x0]) = partition (null . (x3 \\)) x09
    ([x5], [x2]) = partition ((== 1) . length . (x6 \\)) x25

getResult :: ([DigitDisplay], [DigitDisplay]) -> Integer
getResult (xs, rs) = foldl (\a b -> 10 * a + b) 0 $ map (fromJust . flip Map.lookup ms) rs
  where
    ms = findMapping xs

solve2 :: Input -> [Integer]
solve2 = map getResult

print2 :: [Integer] -> String
print2 = show . sum