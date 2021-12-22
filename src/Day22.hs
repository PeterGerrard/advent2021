module Day22 where

import Data.List.Split

type Range = (Integer, Integer)

type Instruction = (Bool, Region)

type Region = (Range, Range, Range)

parseInstruction :: Maybe Range -> String -> Instruction
parseInstruction r s = (t == "on", (xr, yr, zr))
  where
    [(t, ' ' : xs)] = lex s
    [xr, yr, zr] = map (parseRange r) $ splitOn "," xs

parseRange :: Maybe Range -> String -> Range
parseRange r s = maybe (read x1, read x2) (\(mn, mx) -> (max mn $ read x1, min mx $ read x2)) r
  where
    [x1, x2] = splitOn ".." $ drop 2 s

parse :: Maybe Range -> String -> [Instruction]
parse r = map (parseInstruction r) . lines

rangeSize :: Range -> Integer
rangeSize (mn, mx) = max 0 (mx - mn + 1)

regionSize :: Region -> Integer
regionSize (xr, yr, zr) = rangeSize xr * rangeSize yr * rangeSize zr

solve :: [Instruction] -> Integer
solve = sum . map (regionSize . fst) . filter snd . foldl applyInstruction []

cut :: Range -> Range -> ([Range], [Range])
cut (min1, max1) (min2, max2)
  | min1 > max2 || max1 < min2 = ([(min1, max1)], [])
  | min1 >= min2 && max1 <= max2 = ([], [(min1, max1)])
  | min1 < min2 && max1 > max2 = ([(min1, min2 -1), (max2 + 1, max1)], [(min2, max2)])
  | min1 < min2 = ([(min1, min2 -1)], [(min2, max1)])
  | otherwise = ([(max2 + 1, max1)], [(min1, max2)])

remove :: (Region, Bool) -> Region -> [(Region, Bool)]
remove ((xr1, yr1, zr1), b) (xr2, yr2, zr2) = [((xr, yr1, zr1), b) | xr <- xrs] ++ [((xr, yr, zr1), b) | xr <- xrs', yr <- yrs] ++ [((xr, yr, zr), b) | xr <- xrs', yr <- yrs', zr <- zrs]
  where
    (xrs, xrs') = cut xr1 xr2
    (yrs, yrs') = cut yr1 yr2
    (zrs, zrs') = cut zr1 zr2

applyInstruction :: [(Region, Bool)] -> Instruction -> [(Region, Bool)]
applyInstruction rs (b, r) = (r, b) : concatMap (`remove` r) rs
