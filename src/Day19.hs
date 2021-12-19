{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Day19 where

import Data.Either (partitionEithers)
import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
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

reorientate :: Transformation -> [Coord] -> [Coord]
reorientate t = map (apply t)

translateMatrix :: Coord -> Transformation
translateMatrix c = (ident === 0) ||| col (c & 1)

-- Get all the offsets that will move at least 12 coordinates from s2 to s1
overlap :: [Coord] -> [Coord] -> [Coord]
overlap s1 s2 = Map.keys . Map.filter (>= 12) $ Map.fromListWith (+) [(p1 - p2, 1) | p1 <- s1, p2 <- s2]

-- get the larger grid by aligning two scanners
align2 :: [Coord] -> [Coord] -> [([Coord], Coord)]
align2 a b = [(map (off +) o, off) | o <- map (`reorientate` b) orientations, off <- overlap a o]

parse :: String -> [[Coord]]
parse = map parseScanner . splitOn [""] . lines
  where
    parseScanner :: [String] -> [Coord]
    parseScanner = map parseVec . drop 1
    parseVec v = vec3 (read a) (read b) (read c)
      where
        [a, b, c] = splitOn "," v

align :: [[Coord]] -> [([Coord], Coord)]
align (x : xs) = go [(x, vec3 0 0 0)] [x] xs
  where
    go result _ [] = result
    go result (y : ys) zs = go (found ++ result) (map fst found ++ ys) notFound
      where
        (found, notFound) =
          partitionEithers
            [ maybe (Right scanner) Left . safeHead $ align2 y scanner
              | scanner <- zs
            ]

safeHead :: [a] -> Maybe a
safeHead s = if null s then Nothing else Just (head s)

solve1 :: [[Coord]] -> Integer
solve1 = toInteger . length . nub . concatMap fst . align

solve2 :: [[Coord]] -> Double
solve2 cs = maximum [(<.> 1) $ abs (b2 - b1) | b1 <- bs, b2 <- bs]
  where
    bs = map snd $ align cs

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