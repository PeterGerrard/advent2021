module Day12 where

import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

data Cave = Start | End | Large String | Small String deriving (Eq, Show, Ord)

instance Read Cave where
  readsPrec _ s = [(cave, r)]
    where
      [(c, r)] = lex s
      cave
        | c == "start" = Start
        | c == "end" = End
        | all isUpper c = Large c
        | all isLower c = Small c
        | otherwise = error "Not a cave"

data Edge = Edge Cave Cave deriving (Eq, Show)

instance Read Edge where
  readsPrec _ s = [(Edge c1 (read s2), "")]
    where
      [(c1, '-' : s2)] = reads s

add :: Ord k => k -> a -> Map k [a] -> Map k [a]
add key value = Map.alter (Just . maybe [value] (value :)) key

toMap :: [Edge] -> Map Cave [Cave]
toMap = foldl (\m (Edge c1 c2) -> add c1 c2 (add c2 c1 m)) Map.empty

parse :: String -> Map Cave [Cave]
parse = toMap . map read . lines

extendRoute :: [Cave] -> Map Cave [Cave] -> ([Cave] -> Bool) -> Set [Cave]
extendRoute (x : xs) m isValid = Set.fromList $ filter isValid $ map (: x : xs) (fromMaybe [] (Map.lookup x m))

findRoutes :: Map Cave [Cave] -> ([Cave] -> Bool) -> Set [Cave]
findRoutes m isValid = Set.map reverse $ go (Set.singleton [Start]) Set.empty
  where
    go :: Set [Cave] -> Set [Cave] -> Set [Cave]
    go ps rs = if null ps then rs else go (Set.unions $ Set.map (\x -> extendRoute x m isValid) toExtend) (Set.union rs complete)
      where
        (complete, toExtend) = Set.partition ((== End) . head) ps

print :: Integer -> String
print = show

visitSmallAtMost :: Integer -> [Cave] -> Bool
visitSmallAtMost n xs = length start == 1 && length end <= 1 && smallIsValid
  where
    (start, end, small) =
      foldl
        ( \(a1, a2, a3) x -> case x of
            Start -> (x : a1, a2, a3)
            End -> (a1, x : a2, a3)
            Small _ -> (a1, a2, x : a3)
            Large _ -> (a1, a2, a3)
        )
        ([], [], [])
        xs
    multiVisitSmall = filter (> 1) . map (toInteger . length) . group $ sort small
    smallIsValid = length multiVisitSmall <= 1 && all (<= n) multiVisitSmall

solve1 :: Map Cave [Cave] -> Integer
solve1 = toInteger . length . (`findRoutes` visitSmallAtMost 1)

solve2 :: Map Cave [Cave] -> Integer
solve2 = toInteger . length . (`findRoutes` visitSmallAtMost 2)
