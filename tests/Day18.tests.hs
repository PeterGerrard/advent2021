module Main (main) where

import qualified Day18
import System.Exit
import Test.HUnit

main :: IO ()
main = do
  counts2 <-
    runTestTT
      ( test
          [ TestCase (assertEqual "explode 1" (read "[[[[0,9],2],3],4]") (Day18.explode $ read "[[[[[9,8],1],2],3],4]")),
            TestCase (assertEqual "explode 2" (read "[7,[6,[5,[7,0]]]]") (Day18.explode $ read "[7,[6,[5,[4,[3,2]]]]]")),
            TestCase (assertEqual "explode 3" (read "[[6,[5,[7,0]]],3]") (Day18.explode $ read "[[6,[5,[4,[3,2]]]],1]")),
            TestCase (assertEqual "explode 4, left" (read "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]") (Day18.explode $ read "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")),
            TestCase (assertEqual "explode 5" (read "[[3,[2,[8,0]]],[9,[5,[7,0]]]]") (Day18.explode $ read "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")),
            TestCase (assertEqual "split 1" (read "[5,5]") (Day18.split $ read "10")),
            TestCase (assertEqual "split 2" (read "[5,6]") (Day18.split $ read "11")),
            TestCase (assertEqual "split 3" (read "[[[5,5],0],0]") (Day18.split $ read "[[10,0],0]")),
            TestCase (assertEqual "split 4, left" (read "[[5,5],10]") (Day18.split $ read "[10,10]")),
            TestCase (assertEqual "add 1" (read "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]") (Day18.add (read "[[[[4,3],4],4],[7,[[8,4],9]]]") (read "[1,1]"))),
            TestCase (assertEqual "add 2" (read "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]") (Day18.add (read "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]") (read "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"))),
            TestCase (assertEqual "add 3" (read "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]") (Day18.add (read "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]") (read "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"))),
            TestCase (assertEqual "add 4" (read "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]") (Day18.add (read "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]") (read "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"))),
            TestCase (assertEqual "add 5" (read "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]") (Day18.add (read "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]") (read "[7,[5,[[3,8],[1,4]]]]"))),
            TestCase (assertEqual "add 6" (read "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]") (Day18.add (read "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]") (read "[[2,[2,2]],[8,[8,1]]]"))),
            TestCase (assertEqual "add 7" (read "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]") (Day18.add (read "[[[[6,6],[6,6]],[[6,0],[6,7]]],[[[7,7],[8,9]],[8,[8,1]]]]") (read "[2,9]"))),
            TestCase (assertEqual "add 8" (read "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]") (Day18.add (read "[[[[6,6],[7,7]],[[0,7],[7,7]]],[[[5,5],[5,6]],9]]") (read "[1,[[[9,3],9],[[9,0],[0,7]]]]"))),
            TestCase (assertEqual "add 9" (read "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]") (Day18.add (read "[[[[7,8],[6,7]],[[6,8],[0,8]]],[[[7,7],[5,0]],[[5,5],[5,6]]]]") (read "[[[5,[7,4]],7],1]"))),
            TestCase (assertEqual "add 10" (read "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]") (Day18.add (read "[[[[7,7],[7,7]],[[8,7],[8,7]]],[[[7,0],[7,7]],9]]") (read "[[[[4,2],2],6],[8,7]]")))
          ]
      )
  if errors counts2 + failures counts2 == 0 then exitSuccess else exitFailure
