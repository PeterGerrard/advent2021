import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11 --imports
import System.Environment

padLeft :: Int -> a -> [a] -> [a]
padLeft n x xs = replicate (n - length xs) x ++ xs

getSolve1 :: Integer -> (String -> String)
getSolve1 n = case n of
  1 -> Day01.print . Day01.solve1 . Day01.parse
  2 -> Day02.print . Day02.solve1 . Day02.parse
  3 -> Day03.print . Day03.solve1 . Day03.parse
  4 -> Day04.print . Day04.solve1 . Day04.parse
  5 -> Day05.print . Day05.solve1 . Day05.parse
  6 -> Day06.print . Day06.solve1 . Day06.parse
  7 -> Day07.print . Day07.solve1 . Day07.parse
  8 -> Day08.print1 . Day08.solve1 . Day08.parse
  9 -> Day09.print . Day09.solve1 . Day09.parse
  10 -> Day10.print . Day10.solve1 . Day10.parse
  11 -> Day11.print . Day11.solve1 . Day11.parse --solve1Insert

getSolve2 :: Integer -> (String -> String)
getSolve2 n = case n of
  1 -> Day01.print . Day01.solve2 . Day01.parse
  2 -> Day02.print . Day02.solve2 . Day02.parse
  3 -> Day03.print . Day03.solve2 . Day03.parse
  4 -> Day04.print . Day04.solve2 . Day04.parse
  5 -> Day05.print . Day05.solve2 . Day05.parse
  6 -> Day06.print . Day06.solve2 . Day06.parse
  7 -> Day07.print . Day07.solve2 . Day07.parse
  8 -> Day08.print2 . Day08.solve2 . Day08.parse
  9 -> Day09.print . Day09.solve2 . Day09.parse
  10 -> Day10.print . Day10.solve2 . Day10.parse
  11 -> Day11.print . Day11.solve2 . Day11.parse --solve2Insert

getSolver :: Integer -> (Integer -> (String -> String))
getSolver n = if n == 1 then getSolve1 else getSolve2

inputPath :: Integer -> FilePath
inputPath n = "./inputs/Day" ++ padLeft 2 '0' (show n) ++ "." ++ "txt"

main = do
  (d : p : _) <- getArgs
  let (day, part) = (read d, read p)
  input <- readFile $ inputPath day
  putStr $ getSolver part day input