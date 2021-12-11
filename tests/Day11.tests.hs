module Main (main) where

import qualified Day11
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  counts2 <-
    runTestTT
      ( test
          [ TestCase (assertEqual "runDay updates everything by 1" (Day11.toMap [[2, 3], [1, 5]]) (Day11.applyDay $ Day11.toMap [[1, 2], [0, 4]])),
            TestCase (assertEqual "runDay resets On Flash" (Day11.toMap [[0]]) (Day11.applyDay $ Day11.toMap [[9]])),
            TestCase (assertEqual "runDay flash infect adjacent" (Day11.toMap [[2, 2, 2], [2, 0, 2], [2, 2, 2]]) (Day11.applyDay $ Day11.toMap [[0, 0, 0], [0, 9, 0], [0, 0, 0]])),
            TestCase (assertEqual "runDay flash will cascade" (Day11.toMap [[0, 0]]) (Day11.applyDay $ Day11.toMap [[9, 8]])),
            TestCase (assertEqual "runDay example 1 step" (Day11.parse "6594254334\n3856965822\n6375667284\n7252447257\n7468496589\n5278635756\n3287952832\n7993992245\n5957959665\n6394862637") (Day11.applyDay $ Day11.parse "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")),
            TestCase (assertEqual "runDay example 2 step" (Day11.parse "8807476555\n5089087054\n8597889608\n8485769600\n8700908800\n6600088989\n6800005943\n0000007456\n9000000876\n8700006848") (Day11.applyDay $ Day11.applyDay $ Day11.parse "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526"))
          ]
      )
  if errors counts2 + failures counts2 == 0 then exitSuccess else exitFailure
