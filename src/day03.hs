import Data.List

type ReportLine = [Bool]

type Report = [ReportLine]

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

zipListWith :: ([a] -> b) -> [[a]] -> [b]
zipListWith f = map f . transpose

calculateBit :: [Bool] -> Bool
calculateBit = (>= 0) . uncurry (-) . both length . partition (== True)

toBinary :: [Bool] -> Integer
toBinary = foldl (\a b -> 2 * a + (if b then 1 else 0)) 0

calculateGamma :: Report -> Integer
calculateGamma = toBinary . zipListWith calculateBit

solve1 :: Report -> Integer
solve1 r = gamma * epsilon
  where
    gamma = calculateGamma r
    epsilon = 2 ^ length (head r) - 1 - gamma

filterReport :: ([Bool] -> Bool) -> Report -> ReportLine
filterReport _ [x] = x
filterReport f r = bit : filterReport f (map tail (filter ((== bit) . head) r))
  where
    bit = f (map head r)

solve2 :: Report -> Integer
solve2 r = oxygen * co2
  where
    oxygen = toBinary $ filterReport calculateBit r
    co2 = toBinary $ filterReport (not . calculateBit) r

main = interact $ show . solve2 . map (map (== '1')) . lines