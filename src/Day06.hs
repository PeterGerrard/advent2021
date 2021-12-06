module Day06 where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

type LanternFish = Integer

applyDay :: Map LanternFish Integer -> Map LanternFish Integer
applyDay ls = foldl applyInner Map.empty $ Map.keys ls
  where
    applyInner m d = if d == 0 then Map.insertWith (+) 6 x (Map.insertWith (+) 8 x m) else Map.insertWith (+) (d - 1) x m
      where
        x = fromMaybe 0 $ Map.lookup d ls

solve1 :: Map LanternFish Integer -> Map LanternFish Integer
solve1 = last . take 81 . iterate applyDay

solve2 :: Map LanternFish Integer -> Map LanternFish Integer
solve2 = last . take 257 . iterate applyDay

parse :: String -> Map LanternFish Integer
parse = foldl (flip $ Map.alter (Just . maybe 1 (+ 1))) Map.empty . map read . splitOn ","

print :: Map LanternFish Integer -> String
print = show . Map.foldl (+) 0
