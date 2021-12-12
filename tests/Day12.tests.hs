module Main (main) where

import qualified Data.Set as Set
import qualified Day12
import System.Exit
import Test.HUnit

example1 = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"

answer1 = (10, 36)

example2 = "dc-end\nHN-start\nstart-kj\ndc-start\ndc-HN\nLN-dc\nHN-end\nkj-sa\nkj-HN\nkj-dc"

answer2 = (19, 103)

example3 = "fs-end\nhe-DX\nfs-he\nstart-DX\npj-DX\nend-zg\nzg-sl\nzg-pj\npj-he\nRW-he\nfs-DX\npj-RW\nzg-RW\nstart-pj\nhe-WI\nzg-he\npj-fs\nstart-RW"

answer3 = (226, 3509)

main :: IO ()
main = do
  counts2 <-
    runTestTT
      ( test
          [ TestCase (assertEqual "readCave" Day12.Start (read "start")),
            TestCase (assertEqual "readCave" Day12.End (read "end")),
            TestCase (assertEqual "readCave" (Day12.Large "A") (read "A")),
            TestCase (assertEqual "readCave" (Day12.Small "a") (read "a")),
            TestCase
              (assertEqual "parseEdge start to end" (Day12.Edge Day12.Start Day12.End) (read "start-end")),
            TestCase
              (assertEqual "parseEdge small to large" (Day12.Edge (Day12.Small "a") (Day12.Large "B")) (read "a-B")),
            TestCase
              (assertEqual "findRoutes simple" (Set.singleton [Day12.Start, Day12.End]) (Day12.findRoutes (Day12.toMap [Day12.Edge Day12.Start Day12.End]) (Day12.visitSmallAtMost 1))),
            TestCase
              (assertEqual "findRoutes single step" (Set.singleton [Day12.Start, Day12.Small "a", Day12.End]) (Day12.findRoutes (Day12.toMap [Day12.Edge Day12.Start (Day12.Small "a"), Day12.Edge (Day12.Small "a") Day12.End]) (Day12.visitSmallAtMost 1))),
            TestCase
              (assertEqual "findRoutes multiple routes" 2 (length $ Day12.findRoutes (Day12.toMap [Day12.Edge Day12.Start (Day12.Small "a"), Day12.Edge (Day12.Small "a") Day12.End, Day12.Edge Day12.Start (Day12.Small "b"), Day12.Edge (Day12.Small "b") Day12.End]) (Day12.visitSmallAtMost 1))),
            TestCase
              (assertEqual "findRoutes visit Large multiple" 2 (length $ Day12.findRoutes (Day12.toMap [Day12.Edge Day12.Start (Day12.Large "A"), Day12.Edge (Day12.Large "A") (Day12.Small "b"), Day12.Edge (Day12.Large "A") Day12.End]) (Day12.visitSmallAtMost 1))),
            TestCase
              (assertEqual "findRoutes visit small once" 1 (length $ Day12.findRoutes (Day12.toMap [Day12.Edge Day12.Start (Day12.Small "a"), Day12.Edge (Day12.Small "a") (Day12.Small "b"), Day12.Edge (Day12.Small "a") Day12.End]) (Day12.visitSmallAtMost 1))),
            TestCase (assertEqual "solve1 example 1" (fst answer1) (Day12.solve1 $ Day12.parse example1)),
            TestCase (assertEqual "solve1 example 2" (fst answer2) (Day12.solve1 $ Day12.parse example2)),
            TestCase (assertEqual "solve1 example 3" (fst answer3) (Day12.solve1 $ Day12.parse example3)),
            TestCase
              (assertEqual "findRoutes visit small twice" 2 (length $ Day12.findRoutes (Day12.toMap [Day12.Edge Day12.Start (Day12.Small "a"), Day12.Edge (Day12.Small "a") (Day12.Small "b"), Day12.Edge (Day12.Small "a") Day12.End]) (Day12.visitSmallAtMost 2))),
            TestCase (assertEqual "solve2 example 1" (snd answer1) (Day12.solve2 $ Day12.parse example1)),
            TestCase (assertEqual "solve2 example 2" (snd answer2) (Day12.solve2 $ Day12.parse example2)),
            TestCase (assertEqual "solve2 example 3" (snd answer3) (Day12.solve2 $ Day12.parse example3))
          ]
      )
  if errors counts2 + failures counts2 == 0 then exitSuccess else exitFailure
