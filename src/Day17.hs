module Day17 where

parse :: String -> ((Integer, Integer), (Integer, Integer))
parse s = ((minx, maxx), (miny, maxy))
  where
    s1 = drop 15 s
    [(minx, '.' : '.' : s2)] = reads s1
    [(maxx, ',' : ' ' : 'y' : '=' : s3)] = reads s2
    [(miny, '.' : '.' : s4)] = reads s3
    maxy = read s4

print :: Integer -> String
print = show

getPathX :: Integer -> [Integer]
getPathX = go 0
  where
    go x 0 = repeat x
    go x v = x : go (x + v) (v - 1)

getPathY :: Integer -> [Integer]
getPathY = go 0
  where
    go y v = y : go (y + v) (v - 1)

solve :: ((Integer, Integer), (Integer, Integer)) -> [(Integer, Integer)]
solve ((minx, maxx), (miny, maxy)) = [(a, b) | a <- [0 .. maxx], b <- [miny .. (- miny)], any inbounds $ takeWhile (\(x, y) -> x <= maxx && y >= miny) $ zip (getPathX a) (getPathY b)]
  where
    inbounds (x, y) = minx <= x && x <= maxx && miny <= y && y <= maxy

solve1 :: ((Integer, Integer), (Integer, Integer)) -> Integer
solve1 = (\n -> n * (n + 1) `div` 2) . maximum . map snd . solve

solve2 :: ((Integer, Integer), (Integer, Integer)) -> Integer
solve2 = toInteger . length . solve
