module Main (main) where

import qualified Day10
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  counts2 <-
    runTestTT
      ( test
          [ TestCase (assertEqual "Single open" (Day10.Incomplete ")") (Day10.parseLine "(")),
            TestCase (assertEqual "Single close" (Day10.Corrupt ')') (Day10.parseLine ")"))
          ]
      )
  if errors counts2 + failures counts2 == 0 then exitSuccess else exitFailure