module Day02 where

import Data.List.Split

data Submarine = Submarine Integer Integer Integer deriving (Show)

data Instruction = Forward Integer | Down Integer | Up Integer
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s = case x of
  "forward" -> Forward (read y)
  "up" -> Up (read y)
  "down" -> Down (read y)
  where
    [x, y] = splitOn " " s

applyInstruction1 :: Submarine -> Instruction -> Submarine
applyInstruction1 (Submarine h d _) (Forward x) = Submarine (h + x) d 0
applyInstruction1 (Submarine h d _) (Up x) = Submarine h (d - x) 0
applyInstruction1 (Submarine h d _) (Down x) = Submarine h (d + x) 0

applyInstruction2 :: Submarine -> Instruction -> Submarine
applyInstruction2 (Submarine h d a) (Forward x) = Submarine (h + x) (d + x * a) a
applyInstruction2 (Submarine h d a) (Up x) = Submarine h d (a - x)
applyInstruction2 (Submarine h d a) (Down x) = Submarine h d (a + x)

print :: Submarine -> String
print = show . (\(Submarine x d _) -> x * d)

parse :: String -> [Instruction]
parse = map parseInstruction . lines

solve1 :: [Instruction] -> Submarine
solve1 = foldl applyInstruction1 (Submarine 0 0 0)

solve2 :: [Instruction] -> Submarine
solve2 = foldl applyInstruction2 (Submarine 0 0 0)