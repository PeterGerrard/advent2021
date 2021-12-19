{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Day19 where

import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.TypeLits
import Numeric.LinearAlgebra.Static

type Coord = R 3

type Transformation = L 4 4

instance (KnownNat a) => Eq (R a) where
  (==) r1 r2 = extract r1 == extract r2

instance (KnownNat a) => Ord (R a) where
  (<=) r1 r2 = extract r1 <= extract r2

instance (KnownNat a) => Eq (L a a) where
  (==) r1 r2 = extract r1 == extract r2

instance (KnownNat a) => Ord (L a a) where
  (<=) r1 r2 = toRows r1 <= toRows r2

ident :: (KnownNat a) => L a a
ident = build (\r c -> if r == c then 1 else 0)

apply :: Transformation -> Coord -> Coord
apply t = fst . split . (t #>) . (& 1)

reorientate :: Transformation -> Set Coord -> Set Coord
reorientate t = Set.map (apply t)

translateMatrix :: Coord -> Transformation
translateMatrix c = (ident === 0) ||| col (c & 1)

overlap :: Set Coord -> Set Coord -> Set (Set Coord)
overlap s1 s2 = Set.map (Set.union s1) $ Set.filter ((>= 12) . Set.size . Set.intersection s1) ts2
  where
    ts2 = Set.fromList $ map (`reorientate` s2) ts
    ts = [translateMatrix (p1 - apply r p2) Numeric.LinearAlgebra.Static.<> r | r <- orientations, p1 <- Set.toList s1, p2 <- Set.toList s2]

parse :: String -> [Set Coord]
parse = map (Set.fromList . parseScanner) . splitOn [""] . lines
  where
    parseScanner :: [String] -> [Coord]
    parseScanner = map parseVec . drop 1
    parseVec v = vec3 (read a) (read b) (read c)
      where
        [a, b, c] = splitOn "," v

print :: Integer -> String
print = show

tryAdd :: Set Coord -> Set Coord -> Maybe (Set (Set Coord))
tryAdd a = (\x -> if null x then Nothing else Just x) . overlap a

solve :: [Set Coord] -> [Set Coord]
solve (x : xs) = go [x] xs []
  where
    go ys [] [] = ys
    go ys [] zs = go ys zs []
    go ys (x : xs) zs = case mapMaybe (tryAdd x) ys of
      [] -> go ys xs (x : zs)
      ys' -> go (Set.toList $ Set.unions ys') xs zs

solve1 :: [Set Coord] -> Integer
solve1 = toInteger . Set.size . head . solve

solve2 :: [Set Coord] -> Integer
solve2 = const 1

{- ORMOLU_DISABLE -}
orientations :: [Transformation]
orientations = [ matrix
 [ -1 ,  0 , 0 , 0
 ,  0 , -1 , 0 , 0
 ,  0 ,  0 , 1 , 0
 ,  0 ,  0 , 0 , 1  ] , matrix
 [ -1 ,  0 ,  0 , 0
 ,  0 ,  0 , -1 , 0
 ,  0 , -1 ,  0 , 0
 ,  0 ,  0 ,  0 , 1  ] , matrix
 [ -1 , 0 , 0 , 0
 ,  0 , 0 , 1 , 0
 ,  0 , 1 , 0 , 0
 ,  0 , 0 , 0 , 1  ] , matrix
 [ -1 , 0 ,  0 , 0
 ,  0 , 1 ,  0 , 0
 ,  0 , 0 , -1 , 0
 ,  0 , 0 ,  0 , 1  ] , matrix
 [  0 , -1 ,  0 , 0
 , -1 ,  0 ,  0 , 0
 ,  0 ,  0 , -1 , 0
 ,  0 ,  0 ,  0 , 1  ] , matrix
 [ 0 , -1 ,  0 , 0
 , 0 ,  0 , -1 , 0
 , 1 ,  0 ,  0 , 0
 , 0 ,  0 ,  0 , 1  ] , matrix
 [  0 , -1 , 0 , 0
 ,  0 ,  0 , 1 , 0
 , -1 ,  0 , 0 , 0
 ,  0 ,  0 , 0 , 1  ] , matrix
 [ 0 , -1 , 0 , 0
 , 1 ,  0 , 0 , 0
 , 0 ,  0 , 1 , 0
 , 0 ,  0 , 0 , 1  ] , matrix
 [  0 , 0 , -1 , 0
 , -1 , 0 ,  0 , 0
 ,  0 , 1 ,  0 , 0
 ,  0 , 0 ,  0 , 1  ] , matrix
 [  0 ,  0 , -1 , 0
 ,  0 , -1 ,  0 , 0
 , -1 ,  0 ,  0 , 0
 ,  0 ,  0 ,  0 , 1  ] , matrix
 [ 0 , 0 , -1 , 0
 , 0 , 1 ,  0 , 0
 , 1 , 0 ,  0 , 0
 , 0 , 0 ,  0 , 1  ] , matrix
 [ 0 ,  0 , -1 , 0
 , 1 ,  0 ,  0 , 0
 , 0 , -1 ,  0 , 0
 , 0 ,  0 ,  0 , 1  ] , matrix
 [  0 ,  0 , 1 , 0
 , -1 ,  0 , 0 , 0
 ,  0 , -1 , 0 , 0
 ,  0 ,  0 , 0 , 1  ] , matrix
 [ 0 ,  0 , 1 , 0
 , 0 , -1 , 0 , 0
 , 1 ,  0 , 0 , 0
 , 0 ,  0 , 0 , 1  ] , matrix
 [  0 , 0 , 1 , 0
 ,  0 , 1 , 0 , 0
 , -1 , 0 , 0 , 0
 ,  0 , 0 , 0 , 1  ] , matrix
 [ 0 , 0 , 1 , 0
 , 1 , 0 , 0 , 0
 , 0 , 1 , 0 , 0
 , 0 , 0 , 0 , 1  ] , matrix
 [  0 , 1 , 0 , 0
 , -1 , 0 , 0 , 0
 ,  0 , 0 , 1 , 0
 ,  0 , 0 , 0 , 1  ] , matrix
 [  0 , 1 ,  0 , 0
 ,  0 , 0 , -1 , 0
 , -1 , 0 ,  0 , 0
 ,  0 , 0 ,  0 , 1  ] , matrix
 [ 0 , 1 , 0 , 0
 , 0 , 0 , 1 , 0
 , 1 , 0 , 0 , 0
 , 0 , 0 , 0 , 1  ] , matrix
 [ 0 , 1 ,  0 , 0
 , 1 , 0 ,  0 , 0
 , 0 , 0 , -1 , 0
 , 0 , 0 ,  0 , 1  ] , matrix
 [ 1 ,  0 ,  0 , 0
 , 0 , -1 ,  0 , 0
 , 0 ,  0 , -1 , 0
 , 0 ,  0 ,  0 , 1  ] , matrix
 [ 1 , 0 ,  0 , 0
 , 0 , 0 , -1 , 0
 , 0 , 1 ,  0 , 0
 , 0 , 0 ,  0 , 1  ] , matrix
 [ 1 ,  0 , 0 , 0
 , 0 ,  0 , 1 , 0
 , 0 , -1 , 0 , 0
 , 0 ,  0 , 0 , 1  ] , matrix
 [ 1 , 0 , 0 , 0
 , 0 , 1 , 0 , 0
 , 0 , 0 , 1 , 0
 , 0 , 0 , 0 , 1  ] ]
yaw = matrix [0,-1,0,0,1,0,0,0,0,0,1,0,0,0,0,1] :: L 4 4
yaws = take 4 $ iterate (Numeric.LinearAlgebra.Static.<> yaw) yaw
pitch = matrix [0,0,-1,0,0,1,0,0,1,0,0,0,0,0,0,1] :: L 4 4
pitchs = take 4 $ iterate (Numeric.LinearAlgebra.Static.<> pitch) pitch
roll = matrix [1,0,0,0,0,0,-1,0,0,1,0,0,0,0,0,1] :: L 4 4
rolls = take 4 $ iterate (Numeric.LinearAlgebra.Static.<> roll) roll
{- ORMOLU_ENABLE -}