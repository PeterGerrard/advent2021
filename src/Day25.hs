module Day25 where

import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Integer, Integer)

data Location = Empty | Vert | Horz deriving (Eq, Show)

parseLocation :: Char -> Location
parseLocation '.' = Empty
parseLocation '>' = Horz
parseLocation 'v' = Vert
parseLocation _ = error "Dunno"

parse :: String -> Map Position Location
parse = Map.fromList . concat . zipWith (\y -> zipWith (\x c -> ((x,y),parseLocation c)) [0..]) [0..] . lines

stepVert :: Map Position Location -> Map Position Location
stepVert m = foldl (\m (p1,p2) -> Map.insert p1 Empty (Map.insert p2 Vert m)) m mvs
    where
        ps = Map.keys m
        mvs = [((x,y),(x,(y + 1) `mod` wy)) | (x,y) <- ps, Map.lookup (x,y) m == Just Vert, Map.lookup (x,(y + 1) `mod` wy) m == Just Empty]
        wy = (+1) . snd . maximum $ Map.keys m

stepHorz :: Map Position Location -> Map Position Location
stepHorz m = foldl (\m (p1,p2) -> Map.insert p1 Empty (Map.insert p2 Horz m)) m mvs
    where
        ps = Map.keys m
        mvs = [((x,y),((x+1) `mod` wx,y)) | (x,y) <- ps, Map.lookup (x,y) m == Just Horz, Map.lookup ((x+1) `mod` wx ,y) m == Just Empty]
        wx = (+1) . fst . maximum $ Map.keys m

step :: Map Position Location -> Map Position Location
step = stepVert . stepHorz

noChange :: (Eq a) => [a] -> Int
noChange [] = error "Empty"
noChange (x:xs) = go 1 x xs
    where
        go _ _ [] = error "Always changes"
        go n x (x':xs) = if x == x' then n else go (n+1) x' xs

print :: String -> String
print = id

printMap :: Map Position Location -> String
printMap m = unlines [ [printLoc (m Map.! (x,y)) | x <- [0.. maxx]]| y <- [0.. maxy]]
    where
        (maxx, maxy) = maximum $ Map.keys m
        printLoc Empty = '.'
        printLoc Vert = 'v'
        printLoc Horz = '>'

solve1 :: Map Position Location -> String
solve1 = show . noChange . iterate step

solve2 :: Map Position Location -> String
solve2 = error ""
