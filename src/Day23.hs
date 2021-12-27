module Day23 where

import Algorithm.Search (dijkstra)
import Data.Array
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Burrow = Burrow (Array Int (Maybe Char)) (Array Int (Array Int (Maybe Char))) deriving (Eq, Ord, Show)

emptyBurrow :: Int -> Burrow
emptyBurrow n = Burrow emptyHallway (array (0,10) [(i, if even i && i `mod` 9 /= 0 then emptyRoom else blank) | i <- [0..10]])
    where
        emptyHallway = array (0,10) [(i, Nothing ) | i <- [0..10]]
        emptyRoom = array (0,n-1) [(i, Nothing ) | i <- [0..n-1]]
        blank = array (0,-1) []

parse :: String -> Burrow
parse s = foldl place (emptyBurrow n). concat . zipWith (\y -> zipWith (\x c -> ((x,y),c)) [0..]) [0..] $ ls
    where
        ls = lines s
        n = length ls - 3
        place :: Burrow -> ((Int, Int), Char) -> Burrow
        place b (_,' ') = b
        place b (_,'.') = b
        place b (_,'#') = b
        place (Burrow hall rooms) ((x,1),c) = Burrow (hall // [(x-1, Just c)]) rooms
        place (Burrow hall rooms) ((x,y),c) = Burrow hall (rooms // [(x-1, (rooms ! (x-1)) // [(y-2, Just c)])])

range :: Int -> Int -> [Int]
range x y = if y > x then [x..y] else [y..x]

getMoves :: Burrow -> [Burrow]
getMoves b = toHallwayMoves ++ leaveHallwayMoves
    where
        toHallwayMoves = concatMap (getToHallwayMoves b) [2,4,6,8]
        leaveHallwayMoves = concatMap (getLeaveHallwayMoves b) [0..10]

getToHallwayMoves :: Burrow -> Int -> [Burrow]
getToHallwayMoves (Burrow hall rooms) n = if null ms || all ((==n) . getAmph . snd) ms then [] else [Burrow (newHallway m) newRooms | m <- [0,1,3,5,7,9,10], all (isNothing  . (!) hall) (Day23.range n m)]
    where
        room = rooms ! n
        ms = [(i,c) | (i,Just c) <- assocs room]
        (i,c) = head ms
        newRoom = room // [(i,Nothing)]
        newRooms = rooms // [(n, newRoom)]
        newHallway m = hall // [(m, Just c)]


getLeaveHallwayMoves :: Burrow -> Int -> [Burrow]
getLeaveHallwayMoves (Burrow hall rooms) m = case hall ! m of
    Nothing -> []
    Just c -> [Burrow newHall newRooms | canMoveToRoom && targetRoomFine]
        where
            n = getAmph c
            targetRoom = rooms ! n
            targetRoomFine = all (\x -> isNothing x || x == Just c) targetRoom
            targetRoomNothings = length . filter isNothing $ elems targetRoom
            canMoveToRoom = all (isNothing . (!) hall) (Day23.range n m \\[m])
            newHall = hall // [(m, Nothing)]
            newRoom = targetRoom // [(targetRoomNothings -1, Just  c)]
            newRooms = rooms // [(n, newRoom)]

getAmph 'A' = 2
getAmph 'B' = 4
getAmph 'C' = 6
getAmph 'D' = 8

-- dist (x,y) (x', y') = (abs (x' - x) + abs (y' - y))

isComplete :: Burrow -> Bool
isComplete (Burrow _ rooms) = and [all (==Just c) (rooms ! getAmph c) | c <- "ABCD"]

print :: String -> String
print = id

cost 'A' = 1
cost 'B' = 10
cost 'C' = 100
cost 'D' = 1000

getCost :: Burrow -> Burrow -> Int
getCost (Burrow h1 rs1) (Burrow h2 rs2) = c * (1 + j + abs (m - n))
     where
         m = head [i | i <- [0..10], (h1 ! i) /= (h2 ! i)]
         ch = fromMaybe (fromJust $ h2 ! m) (h1 ! m)
         n = head [i | i <- [0..10], (rs1 ! i) /= (rs2 ! i)]
         r1 = rs1 ! n
         r2 = rs2 ! n
         j = head [i | i <- [0..(snd (bounds r1))], (r1 ! i) /= (r2 ! i)]
         c = cost ch

solve1 :: Burrow -> String
solve1 = show . fst . fromJust . dijkstra getMoves getCost isComplete

solve2 :: Burrow -> String
solve2 = solve1

adjustInput :: String -> String
adjustInput = unlines . (\(a,c) -> a ++ ["  #D#C#B#A#","  #D#B#A#C#"] ++ c) . splitAt 3 . lines
