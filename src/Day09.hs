module Day09 where

import Data.Map (Map)
import qualified Data.Map as Map

parse :: String -> Map (Integer, Integer) Integer
parse = Map.fromList . concat . zipWith (\x xs -> map (\(y, n) -> ((x, y), read [n])) xs) [0 ..] . map (zip [0 ..]) . lines

adjacent :: (Integer, Integer) -> [(Integer, Integer)]
adjacent (x, y) = [(x -1, y), (x + 1, y), (x, y -1), (x, y + 1)]