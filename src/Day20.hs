{-# LANGUAGE TupleSections #-}

module Day20 where

import Control.Arrow
import Data.Either (partitionEithers)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Pixel = Light | Dark deriving (Eq)

readPixel :: Char -> Either Pixel Char
readPixel '#' = Left Light
readPixel '.' = Left Dark
readPixel x = Right x

instance Read Pixel where
  readsPrec _ (x : xs) = case readPixel x of
    Left y -> [(y, xs)]
    Right _ -> []
  readList = (: []) . partitionEithers . map readPixel

instance Show Pixel where
  show Light = "#"
  show Dark = "."

type Position = (Integer, Integer)

parseAlgorithm :: String -> Map Integer Pixel
parseAlgorithm = Map.fromList . zip [0 ..] . read

parseImage :: [String] -> (Map Position Pixel, Pixel)
parseImage = (,Dark) . Map.fromList . concat . zipWith (\y -> zipWith (\x p -> ((x, y), p)) [0 ..] . read) [0 ..]

parse :: String -> (Map Integer Pixel, (Map Position Pixel, Pixel))
parse s = (parseAlgorithm a, parseImage rs)
  where
    (a : _ : rs) = lines s

print :: String -> String
print = id

lookupImage :: Position -> Pixel -> Map Position Pixel -> Pixel
lookupImage p d = fromMaybe d . Map.lookup p

runAlgorithm :: Map Integer Pixel -> Map Position Pixel -> Pixel -> Position -> Pixel
runAlgorithm m i d (x, y) = fromJust $ Map.lookup num m
  where
    ps = map (\p -> lookupImage p d i) [(x -1, y -1), (x, y -1), (x + 1, y -1), (x -1, y), (x, y), (x + 1, y), (x -1, y + 1), (x, y + 1), (x + 1, y + 1)]
    num = foldl (\a b -> 2 * a + if b == Light then 1 else 0) 0 ps

step :: Map Integer Pixel -> (Map Position Pixel, Pixel) -> (Map Position Pixel, Pixel)
step m (i, d) = (Map.fromList [((x, y), runAlgorithm m i d (x, y)) | x <- [minx -1 .. maxx + 1], y <- [miny -1 .. maxy + 1]], snd $ (if d == Dark then Map.findMin else Map.findMax) m)
  where
    (minx, maxx) = (minimum &&& maximum) . map fst $ Map.keys i
    (miny, maxy) = (minimum &&& maximum) . map snd $ Map.keys i

solve :: Int -> (Map Integer Pixel, (Map Position Pixel, Pixel)) -> String
solve n (m, i) = show . Map.size . Map.filter (== Light) . fst . (!! n) $ iterate (step m) i

solve1 :: (Map Integer Pixel, (Map Position Pixel, Pixel)) -> String
solve1 = solve 2

solve2 :: (Map Integer Pixel, (Map Position Pixel, Pixel)) -> String
solve2 = solve 50
