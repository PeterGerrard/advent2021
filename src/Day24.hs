module Day24 where

import Control.Arrow
import Data.List.Split
import Data.Maybe
import Data.SBV
import Data.Char (isLetter)

data Variable = W | X | Y | Z

instance Read Variable where
    readsPrec _ [] = []
    readsPrec _ (c:cs)
        | c == 'w' = [(W,cs)]
        | c == 'x' = [(X,cs)]
        | c == 'y' = [(Y,cs)]
        | c == 'z' = [(Z,cs)]
        | otherwise = []

data Instruction =
    Input Variable
    | Add Variable (Either Variable Int)
    | Mul Variable (Either Variable Int)
    | Div Variable (Either Variable Int)
    | Mod Variable (Either Variable Int)
    | Eql Variable (Either Variable Int)

instance Read Instruction where
    readsPrec _ s
        | i == "inp" = [(Input (read xs),"")]
        | i == "add" = [(Add (read a) b',"")]
        | i == "mul" = [(Mul (read a) b',"")]
        | i == "div" = [(Div (read a) b',"")]
        | i == "mod" = [(Mod (read a) b',"")]
        | i == "eql" = [(Eql (read a) b',"")]
        | otherwise = []
      where
        [(i,' ':xs)] = lex s
        [a,b@(c:_)] = splitOn " " xs
        b' = if isLetter c then Left (read b) else Right (read b)

data ALU = Alu SInt64 SInt64 SInt64 SInt64 deriving (Show)

initialiseAlu :: ALU
initialiseAlu = Alu 0 0 0 0

readVar :: ALU -> Variable -> SInt64
readVar (Alu w _ _ _) W = w
readVar (Alu _ x _ _) X = x
readVar (Alu _ _ y _) Y = y
readVar (Alu _ _ _ z) Z = z

writeVar :: ALU -> Variable -> SInt64 -> ALU
writeVar (Alu w x y z) W n = Alu n x y z
writeVar (Alu w x y z) X n = Alu w n y z
writeVar (Alu w x y z) Y n = Alu w x n z
writeVar (Alu w x y z) Z n = Alu w x y n

getVal :: ALU -> Either Variable Int -> SInt64
getVal _ (Right n) = fromIntegral n
getVal a (Left v) = readVar a v

applyInstruction :: ALU -> Instruction -> [SInt64] -> (ALU, [SInt64])
applyInstruction _ (Input v) [] = error "Not enough input"
applyInstruction a (Input v) (d:ds) = (writeVar a v d, ds)
applyInstruction a i ds = case i of
    Add v w -> (writeVar a v (readVar a v + getVal a w), ds)
    Mul v w -> (writeVar a v (readVar a v * getVal a w), ds)
    Div v w -> (writeVar a v (readVar a v `sDiv` getVal a w), ds)
    Mod v w -> (writeVar a v (readVar a v `sMod` getVal a w), ds)
    Eql v w -> (writeVar a v (oneIf (readVar a v .== getVal a w)), ds)
    Input _ -> error "Unreachable"

run :: ALU -> [Instruction] -> [SInt64] -> ALU
run a [] _ = a
run a (i:is) ds = (\(a',ds') -> run a' is ds') $ applyInstruction a i ds

parse :: String -> [Instruction]
parse = map read . lines

print :: String -> String
print = id

getZ :: ALU -> SInt64
getZ (Alu _ _ _ z) = z

isValid :: [Instruction] -> [SInt64] -> SBool
isValid is inp =
  sAll (\n -> n .>= 1 .&& n .<= 9) inp
    .&& getZ (run initialiseAlu is inp) .== 0

toNum :: [SInt64] -> SInt64
toNum = foldl (\acc e -> acc * 10 + e) 0

solve1 :: [Instruction] -> IO OptimizeResult 
solve1 is = optimize Lexicographic $ do
  numbers <- mkExistVars 14
  constrain $ isValid is numbers
  maximize "value" $ toNum numbers

solve2 :: [Instruction] -> IO OptimizeResult 
solve2 is = optimize Lexicographic $ do
  numbers <- mkExistVars 14
  constrain $ isValid is numbers
  minimize "value" $ toNum numbers