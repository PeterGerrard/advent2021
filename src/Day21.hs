{-# LANGUAGE TupleSections #-}

module Day21 where

import Control.Arrow
import Data.List
import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

newtype Position = P Integer deriving (Show, Eq, Ord)

newtype DieRoll = D Integer deriving (Show, Eq)

move :: Position -> DieRoll -> Position
move (P p) (D n) = P $ (p + n) `mod` 10

score :: Position -> Integer
score (P n) = n + 1

deterministicDie :: [DieRoll]
deterministicDie = concat $ repeat $ map D [1 .. 100]

data Player = Player {getPosition :: Position, getScore :: Integer} deriving (Show)

createPlayer :: Integer -> Player
createPlayer p = Player {getPosition = P p, getScore = 0}

turn :: [Player] -> [DieRoll] -> [Player]
turn (p : ps) ds = ps ++ [Player {getPosition = newPosition, getScore = score newPosition + getScore p}]
  where
    newPosition = foldl move (getPosition p) ds

deterministicGame :: [Player] -> [(Integer, [Player])]
deterministicGame = zip [0 ..] . map fst . iterate t . (,deterministicDie)
  where
    t (ps, ds) = (turn ps (take 3 ds), drop 3 ds)

parsePlayer :: String -> Player
parsePlayer s = createPlayer (read p - 1)
  where
    [_, p] = splitOn ": " s

parse :: String -> [Player]
parse = map parsePlayer . lines

print :: String -> String
print = id

solve1 :: [Player] -> String
solve1 = show . uncurry (*) . ((3 *) *** (minimum . map getScore)) . head . dropWhile (all ((< 1000) . getScore) . snd) . deterministicGame

diracDie :: Map Integer Integer
diracDie = Map.fromListWith (+) [(d1 + d2 + d3, 1) | d1 <- [1, 2, 3], d2 <- [1, 2, 3], d3 <- [1, 2, 3]]

data GameState = Game Integer (Position, Integer) (Position, Integer) deriving (Eq)

instance Ord GameState where
  (<=) (Game t1 a1 b1) (Game t2 a2 b2)
    | t1 /= t2 = t1 <= t2
    | t1 `mod` 2 == 0 = a1 < a2 || (a1 == a2 && b1 <= b2)
    | otherwise = b1 < b2 || (b1 == b2 && a1 <= a2)

diracGame' :: Integer -> Integer -> Map GameState Integer -> (Integer, Integer)
diracGame' p1Won p2Won m
  | Map.size m == 0 = (p1Won, p2Won)
  | otherwise = diracGame' (p1Won + newP1Winners) (p2Won + newP2Winners) (Map.unionWith (+) m' ms)
  where
    ((Game t (p1, s1) (p2, s2), a), m') = Map.deleteFindMin m
    (p1Winners, p1StillPlaying) = partition (\((_, s), _) -> s >= 21) $ if t `mod` 2 == 0 then [((p', s1 + score p'), t) | (d, t) <- Map.toList diracDie, let p' = move p1 (D d)] else [((p1, s1), 1)]
    (p2Winners, p2StillPlaying) = partition (\((_, s), _) -> s >= 21) $ if t `mod` 2 == 1 then [((p', s2 + score p'), t) | (d, t) <- Map.toList diracDie, let p' = move p2 (D d)] else [((p2, s2), 1)]
    ms = Map.fromList [(Game (t + 1) x y, a * x' * y') | (x, x') <- p1StillPlaying, (y, y') <- p2StillPlaying]
    newP1Winners = a * sum (map snd p1Winners)
    newP2Winners = a * sum (map snd p2Winners)

diracGame :: [Player] -> (Integer, Integer)
diracGame [p1, p2] = diracGame' 0 0 $ Map.singleton (Game 0 (getPosition p1, 0) (getPosition p2, 0)) 1

solve2 :: [Player] -> String
solve2 = show . uncurry max . diracGame
