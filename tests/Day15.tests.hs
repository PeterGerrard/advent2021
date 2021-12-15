module Main (main) where

import qualified Day15
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  counts2 <-
    runTestTT
      ( test
          [
              TestCase (assertEqual "Example" 2 (1+1))
          ]
      )
  if errors counts2 + failures counts2 == 0 then exitSuccess else exitFailure
