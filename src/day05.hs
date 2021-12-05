import Data.Map (Map)
import qualified Data.Map as Map

data Coord = Coord Integer Integer deriving (Eq,Ord)

instance Read Coord where
    readsPrec _ s  = [(Coord x y,r)]
        where
            [(x, r1)] = reads s
            [(",", r2)] = lex r1
            [(y, r)] = reads r2

instance Show Coord where
    show (Coord x y) = "(" ++ show x ++ "," ++ show y ++ ")"

data LineSegment = LineSegment Coord Coord

instance Read LineSegment where
    readsPrec _ s  = [(LineSegment x y,r)]
        where
            [(x, r1)] = reads s
            [("->", r2)] = lex r1
            [(y, r)] = reads $ dropWhile (==' ') r2

instance Show LineSegment where
    show (LineSegment x y) = show x ++ " -> " ++ show y

interpolate :: LineSegment -> [Coord]
interpolate (LineSegment (Coord x1 y1) (Coord x2 y2)) = zipWith Coord xs ys
    where
        xs = [x1, x1 + signum (x2 - x1)..x2]
        ys = [y1, y1 + signum (y2 - y1)..y2]


addPositions :: Map Coord Integer -> [Coord] -> Map Coord Integer
addPositions = foldl (flip $ Map.alter (Just . maybe 1 (+1)))

addLineSegment :: Map Coord Integer -> LineSegment -> Map Coord Integer
addLineSegment m = addPositions m . interpolate

solve :: [LineSegment] -> Int
solve = Map.size . Map.filter (>=2) . getGrid

printGrid :: Map Coord Integer -> String
printGrid m = unlines $ map concat [[maybe "." show (Map.lookup (Coord x y) m) | x <- [minx..maxx]] | y <- [miny..maxy]]
    where
        cs = Map.keys m
        minx = minimum $ map (\(Coord x y) -> x) cs
        maxx = maximum $ map (\(Coord x y) -> x) cs
        miny = minimum $ map (\(Coord x y) -> y) cs
        maxy = maximum $ map (\(Coord x y) -> y) cs

isStraight :: LineSegment -> Bool
isStraight (LineSegment (Coord x1 y1) (Coord x2 y2)) = x1 == x2 || y1 == y2

getGrid :: [LineSegment] -> Map Coord Integer
getGrid = foldl addLineSegment Map.empty

solve1 = solve . filter isStraight
solve2 = solve

main = interact $ show . solve2 . map read . lines