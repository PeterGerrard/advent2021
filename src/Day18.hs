{-# LANGUAGE LambdaCase #-}

module Day18 where

import Control.Arrow
import Data.Maybe

data SnailNumber = Terminator Integer | Pair SnailNumber SnailNumber deriving (Eq)

liftMaybe :: (a, Maybe b) -> Maybe (a, b)
liftMaybe (_, Nothing) = Nothing
liftMaybe (a, Just b) = Just (a, b)

instance Read SnailNumber where
  readsPrec _ s = case s of
    ('[' : ss) ->
      concat
        . mapMaybe
          ( \(sn, rs) -> case rs of
              (',' : sss) -> Just $ mapMaybe (liftMaybe . (Pair sn *** (\case (']' : m) -> Just m; _ -> Nothing))) $ reads sss
              _ -> Nothing
          )
        $ reads ss
    _ -> map (first Terminator) $ reads s

instance Show SnailNumber where
  show (Terminator x) = show x
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

insertSnailLeft :: Integer -> SnailNumber -> SnailNumber
insertSnailLeft x (Terminator y) = Terminator $ x + y
insertSnailLeft x (Pair a b) = Pair (insertSnailLeft x a) b

insertSnailLeftM :: Maybe Integer -> SnailNumber -> SnailNumber
insertSnailLeftM Nothing = id
insertSnailLeftM (Just x) = insertSnailLeft x

insertSnailRight :: Integer -> SnailNumber -> SnailNumber
insertSnailRight x (Terminator y) = Terminator $ x + y
insertSnailRight x (Pair a b) = Pair a (insertSnailRight x b)

insertSnailRightM :: Maybe Integer -> SnailNumber -> SnailNumber
insertSnailRightM Nothing = id
insertSnailRightM (Just x) = insertSnailRight x

explode :: SnailNumber -> SnailNumber
explode = (\(f, _, _, _) -> f) . go 0
  where
    go _ x@(Terminator _) = (x, Nothing, Nothing, False)
    go 4 (Pair (Terminator x) (Terminator y)) = (Terminator 0, Just x, Just y, True)
    go n (Pair p1 p2) = if d1 then (Pair x1 (insertSnailLeftM x3 p2), x2, Nothing, True) else (Pair (insertSnailRightM y2 p1) y1, Nothing, y3, d2)
      where
        (x1, x2, x3, d1) = go (n + 1) p1
        (y1, y2, y3, d2) = go (n + 1) p2

split :: SnailNumber -> SnailNumber
split = fst . go
  where
    go (Terminator n) = if n < 10 then (Terminator n, False) else (Pair (Terminator $ n `div` 2) (Terminator $ n - (n `div` 2)), True)
    go (Pair a b) = (\(a1, d) -> if d then (Pair a1 b, True) else first (Pair a1) (go b)) $ go a

reduce :: SnailNumber -> SnailNumber
reduce sn
  | esn /= sn = reduce esn
  | ssn /= sn = reduce ssn
  | otherwise = sn
  where
    esn = explode sn
    ssn = split sn

add :: SnailNumber -> SnailNumber -> SnailNumber
add s1 = reduce . Pair s1

parse :: String -> [SnailNumber]
parse = map read . lines

magnitude :: SnailNumber -> Integer
magnitude (Terminator n) = n
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

print :: Integer -> String
print = show

solve1 :: [SnailNumber] -> Integer
solve1 = magnitude . foldl1 add

solve2 :: [SnailNumber] -> Integer
solve2 xs = maximum [magnitude $ add x1 x2 | x1 <- xs, x2 <- xs, x1 /= x2]
