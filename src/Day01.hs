module Day01 where

parse :: String -> [Integer]
parse = map read . lines

print :: Int -> String
print = show

solve1 :: [Integer] -> Int
solve1 xs = length $ filter (< 0) $ zipWith (-) xs (drop 1 xs)

solve2 :: [Integer] -> Int
solve2 xs = solve1 $ zipWith3 (\a b -> (+) ((+) a b)) xs (drop 1 xs) (drop 2 xs)