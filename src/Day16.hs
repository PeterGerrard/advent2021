module Day16 where

import Control.Arrow
import Data.Maybe

data Packet
  = Number Integer Integer
  | Sum Integer [Packet]
  | Product Integer [Packet]
  | Minimum Integer [Packet]
  | Maximum Integer [Packet]
  | GreaterThan Integer [Packet]
  | LessThan Integer [Packet]
  | EqualTo Integer [Packet]
  deriving (Eq, Show)

newtype BitString = Bits String deriving (Eq, Show)

bitsToInteger :: BitString -> Integer
bitsToInteger (Bits bs) = foldl (\acc c -> 2 * acc + (if c == '1' then 1 else 0)) 0 bs

unhex :: String -> BitString
unhex = Bits . concatMap unhexDigit
  where
    unhexDigit '0' = "0000"
    unhexDigit '1' = "0001"
    unhexDigit '2' = "0010"
    unhexDigit '3' = "0011"
    unhexDigit '4' = "0100"
    unhexDigit '5' = "0101"
    unhexDigit '6' = "0110"
    unhexDigit '7' = "0111"
    unhexDigit '8' = "1000"
    unhexDigit '9' = "1001"
    unhexDigit 'A' = "1010"
    unhexDigit 'B' = "1011"
    unhexDigit 'C' = "1100"
    unhexDigit 'D' = "1101"
    unhexDigit 'E' = "1110"
    unhexDigit 'F' = "1111"
    unhexDigit x = error $ "Not a hexadecimal digit" ++ show x

parseLiteralValue :: BitString -> (Integer, BitString)
parseLiteralValue = parseLiteralValue' 0
  where
    parseLiteralValue' n (Bits ('0' : bs)) = ((n * 16 +) . bitsToInteger . Bits *** Bits) $ splitAt 4 bs
    parseLiteralValue' n (Bits ('1' : bs)) = (\(v, rs) -> parseLiteralValue' (n * 16 + bitsToInteger (Bits v)) (Bits rs)) $ splitAt 4 bs

parseOperator :: BitString -> ([Packet], BitString)
parseOperator (Bits ('0' : bs)) = (fst $ parsePackets' (Bits subPacketsString), Bits rest)
  where
    (subPacketLengthString, r1) = splitAt 15 bs
    subPacketLength = bitsToInteger (Bits subPacketLengthString)
    (subPacketsString, rest) = splitAt (fromInteger subPacketLength) r1
parseOperator (Bits ('1' : bs)) = parsePackets subPacketLength (Bits r1)
  where
    (subPacketLengthString, r1) = splitAt 11 bs
    subPacketLength = bitsToInteger (Bits subPacketLengthString)

parsePacket :: BitString -> (Packet, BitString)
parsePacket (Bits bs) = case t of
  4 -> first (Number version) $ parseLiteralValue (Bits r2)
  0 -> first (Sum version) $ parseOperator (Bits r2)
  1 -> first (Product version) $ parseOperator (Bits r2)
  2 -> first (Minimum version) $ parseOperator (Bits r2)
  3 -> first (Maximum version) $ parseOperator (Bits r2)
  5 -> first (GreaterThan version) $ parseOperator (Bits r2)
  6 -> first (LessThan version) $ parseOperator (Bits r2)
  7 -> first (EqualTo version) $ parseOperator (Bits r2)
  where
    (versionString, r1) = splitAt 3 bs
    (typeString, r2) = splitAt 3 r1
    version = bitsToInteger (Bits versionString)
    t = bitsToInteger (Bits typeString)

parsePackets :: Integer -> BitString -> ([Packet], BitString)
parsePackets 0 bs = ([], bs)
parsePackets n bs = (p : ps, rs)
  where
    (p, r1) = parsePacket bs
    (ps, rs) = parsePackets (n -1) r1

parsePackets' :: BitString -> ([Packet], BitString)
parsePackets' bs@(Bits v) = if all (== '0') v then ([], Bits []) else (p : ps, rs)
  where
    (p, r1) = parsePacket bs
    (ps, rs) = parsePackets' r1

parse :: String -> Packet
parse = fst . parsePacket . unhex

print :: Integer -> String
print = show

getVersions :: Packet -> [Integer]
getVersions (Number v _) = [v]
getVersions (Sum v cs) = v : concatMap getVersions cs
getVersions (Product v cs) = v : concatMap getVersions cs
getVersions (Minimum v cs) = v : concatMap getVersions cs
getVersions (Maximum v cs) = v : concatMap getVersions cs
getVersions (GreaterThan v cs) = v : concatMap getVersions cs
getVersions (LessThan v cs) = v : concatMap getVersions cs
getVersions (EqualTo v cs) = v : concatMap getVersions cs

solve1 :: Packet -> Integer
solve1 = sum . getVersions

evaluate :: Packet -> Integer
evaluate (Number _ v) = v
evaluate (Sum _ cs) = sum $ map evaluate cs
evaluate (Product _ cs) = product $ map evaluate cs
evaluate (Minimum _ cs) = minimum $ map evaluate cs
evaluate (Maximum _ cs) = maximum $ map evaluate cs
evaluate (GreaterThan _ [c1, c2]) = if evaluate c1 > evaluate c2 then 1 else 0
evaluate (LessThan _ [c1, c2]) = if evaluate c1 < evaluate c2 then 1 else 0
evaluate (EqualTo _ [c1, c2]) = if evaluate c1 == evaluate c2 then 1 else 0
evaluate x = error $ show x

solve2 :: Packet -> Integer
solve2 = evaluate
