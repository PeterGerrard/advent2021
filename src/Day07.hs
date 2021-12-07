module Day07 where

import Data.Bits (Bits (xor))
import Data.List (sort)
import Data.List.Split

parse :: String -> [Integer]
parse = map read . splitOn ","

print :: Integer -> String
print = show

middle :: [a] -> a
middle [] = error "Woops"
middle [x] = x
middle (x : ys) = middle (reverse ys)

solve1 :: [Integer] -> Integer
solve1 xs = sum $ map (abs . (m -)) xs
  where
    m = middle $ sort xs

fuelCost :: Integer -> Integer -> Integer
fuelCost x y = div (n * n + n) 2
  where
    n = abs (x - y)

totalFuel :: Integer -> [Integer] -> Integer
totalFuel n = sum . map (fuelCost n)

potentialNumbers :: [Integer] -> [Integer]
potentialNumbers xs = [minimum xs .. maximum xs]

solve2 :: [Integer] -> Integer
solve2 xs = minimum $ map (flip totalFuel xs) $ potentialNumbers xs