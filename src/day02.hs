import Data.List.Split

data Submarine = Submarine Integer Integer Integer deriving (Show)

data Instruction = Forward Integer | Down Integer | Up Integer
    deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction s = case x of
        "forward" -> Forward (read y)
        "up" -> Up (read y)
        "down" -> Down (read y)
    where (x:y:[]) = splitOn " " s

applyInstruction :: Submarine -> Instruction -> Submarine
applyInstruction (Submarine h d a) (Forward x) = Submarine (h + x) (d + x*a) a
applyInstruction (Submarine h d a) (Up x) = Submarine h d (a-x)
applyInstruction (Submarine h d a) (Down x) = Submarine h d (a+x)

main = interact $ show . (\(Submarine x d _) -> x * d) . foldl applyInstruction (Submarine 0 0 0) . map parseInstruction . lines