module Main (main) where

import qualified Day17
import System.Exit
import Test.HUnit
import Test.QuickCheck

prop_nondescendinginfpathX :: NonNegative Integer -> Bool
prop_nondescendinginfpathX (NonNegative x) = notElem GT . take 1000 . (\xs -> zipWith compare xs (drop 1 xs)) $ Day17.getPathX x

main :: IO ()
main =
  quickCheck prop_nondescendinginfpathX
