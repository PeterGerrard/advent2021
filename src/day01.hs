solve1 :: [Integer] -> Integer
solve1 [] = 0
solve1 (x : xs) = snd $ foldl (\(z, c) y -> (y, c + (if y > z then 1 else 0))) (x, 0) xs

solve2 :: [Integer] -> Integer
solve2 xs = solve1 $ zipWith3 (\a b -> (+) ((+) a b)) xs (drop 1 xs) (drop 2 xs)

main = interact $ show . solve2 . map read . lines