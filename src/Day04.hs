module Day04 where

import Data.List
import Data.List.Split

data BingoEntry = Marked Integer | Unmarked Integer deriving (Show)

isMarked :: BingoEntry -> Bool
isMarked (Marked _) = True
isMarked _ = False

unpack :: BingoEntry -> Integer
unpack (Marked x) = x
unpack (Unmarked x) = x

type Bingo = [[BingoEntry]]

hasRow :: Bingo -> Bool
hasRow = any (all isMarked)

hasColumn :: Bingo -> Bool
hasColumn = hasRow . transpose

won :: Bingo -> Bool
won b = hasRow b || hasColumn b

score :: Bingo -> Integer
score = sum . map unpack . filter (not . isMarked) . concat

callNumber :: Integer -> Bingo -> Bingo
callNumber n = map (map (\x -> if unpack x == n then Marked n else x))

applyNumber :: Integer -> [Bingo] -> ([Bingo], [Bingo])
applyNumber n bs = partition won nbs
  where
    nbs = map (callNumber n) bs

solve1 :: ([Integer], [Bingo]) -> Integer
solve1 (n : ns, bs) =
  case applyNumber n bs of
    (w : _, _) -> n * score w
    ([], nbs) -> solve1 (ns, nbs)

toBoard :: [String] -> Bingo
toBoard = map (map (Unmarked . read) . words)

toBoards :: [String] -> [Bingo]
toBoards = map (toBoard . take 5) . chunksOf 6

parse :: String -> ([Integer], [Bingo])
parse s = (map read $ splitOn "," ns, toBoards bs)
  where
    (ns : _ : bs) = lines s

solve2 :: ([Integer], [Bingo]) -> Integer
solve2 (n : ns, bs) =
  case applyNumber n bs of
    (w : _, []) -> n * score w
    (_, nbs) -> solve2 (ns, nbs)

print :: Integer -> String
print = show