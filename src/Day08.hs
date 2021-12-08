module Day08 where

import Data.Bool (Bool (True))
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

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
    [i, d] = map words $ splitOn " | " s

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

solve2 :: Input -> Output
solve2 _ = []

print2 :: Output -> String
print2 _ = ""