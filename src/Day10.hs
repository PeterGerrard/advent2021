module Day10 where

import Data.List

data Parse a = Incomplete [a] | Corrupt a deriving (Eq, Ord, Show)

parseLine :: String -> Parse Char
parseLine = go []
    where
        go xs [] = Incomplete xs
        go xs (y:ys) 
            | y == '(' = go (')':xs) ys
            | y == '[' = go (']':xs) ys
            | y == '{' = go ('}':xs) ys
            | y == '<' = go ('>':xs) ys
            | not (null xs) && head xs == y = go (tail xs) ys
            | otherwise = Corrupt y

parse :: String -> [String]
parse = lines

corruptionScore :: Parse Char -> Integer
corruptionScore (Incomplete _) = 0
corruptionScore (Corrupt ')') = 3
corruptionScore (Corrupt ']') = 57
corruptionScore (Corrupt '}') = 1197
corruptionScore (Corrupt '>') = 25137

solve1 :: [String] -> Integer
solve1 = sum . map (corruptionScore . parseLine)

incompleteScore :: String -> Integer
incompleteScore = foldl (\n x -> 5*n + score x) 0
    where
        score ')' = 1
        score ']' = 2
        score '}' = 3
        score '>' = 4

catIncomplete :: [Parse a] -> [[a]]
catIncomplete ls = [xs | Incomplete xs <- ls]

middle :: Ord a => [a] -> a
middle xs = sort xs !! div (length xs) 2

solve2 :: [String] -> Integer
solve2 = middle . map incompleteScore . catIncomplete . map parseLine

print :: Integer -> String
print = show