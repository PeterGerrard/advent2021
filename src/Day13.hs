{-# LANGUAGE TupleSections #-}

module Day13 where

import Control.Arrow
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Set as Set

data Fold = Vertical Integer | Horizontal Integer

instance Read Fold where
  readsPrec _ s = [((if t == "y" then Horizontal else Vertical) (read c), "")]
    where
      (t, _ : c) = splitAt 1 $ drop 11 s

parseLine :: String -> (Integer, Integer)
parseLine s = (read x, read y)
  where
    [x, y] = splitOn "," s

parseMap :: [String] -> Map (Integer, Integer) Bool
parseMap = populate . Map.fromList . map ((,True) . parseLine) . filter (not . null)
  where
    populate m = Map.fromList $ map (\p -> (p, lookupWithDefault False p m)) ks
      where
        maxx = maximum . map fst $ Map.keys m
        maxy = maximum . map snd $ Map.keys m
        ks = [(x, y) | x <- [0 .. maxx], y <- [0 .. maxy]]

printMap :: Map (Integer, Integer) Bool -> String
printMap m = unlines [[maybe ' ' (\z -> if z then '#' else ' ') (Map.lookup (x, y) m) | x <- [minx .. maxx]] | y <- [miny .. maxy]]
  where
    minx = minimum . map fst $ Map.keys m
    maxx = maximum . map fst $ Map.keys m
    miny = minimum . map snd $ Map.keys m
    maxy = maximum . map snd $ Map.keys m

parse :: String -> (Map (Integer, Integer) Bool, [Fold])
parse = (parseMap *** map read) . (\[a, b] -> (a, b)) . splitOn [""] . lines

lookupWithDefault :: Ord k => a -> k -> Map k a -> a
lookupWithDefault d k = fromMaybe d . Map.lookup k

applyFold :: Map (Integer, Integer) Bool -> Fold -> Map (Integer, Integer) Bool
applyFold m f =
  (`Map.restrictKeys` ks) $
    Map.mapWithKey
      ( \(x, y) v ->
          if x < foldx && y < foldy && not v
            then lookupWithDefault False (foldx + (foldx - x), y) m || lookupWithDefault False (x, foldy + (foldy - y)) m
            else v
      )
      m
  where
    (foldx, foldy) = case f of
      Vertical n -> (n, maxy)
      Horizontal n -> (maxx, n)
    maxx = (1 +) . maximum . map fst $ Map.keys m
    maxy = (1 +) . maximum . map snd $ Map.keys m
    ks = Set.fromList [(x, y) | x <- [0 .. foldx - 1], y <- [0 .. foldy - 1]]

print :: Int -> String
print = show

print2 :: Map (Integer, Integer) Bool -> String
print2 = printMap

solve1 :: (Map (Integer, Integer) Bool, [Fold]) -> Int
solve1 (m, f : _) = Map.size . Map.filter id $ applyFold m f

solve2 :: (Map (Integer, Integer) Bool, [Fold]) -> Map (Integer, Integer) Bool
solve2 = uncurry (foldl applyFold)
