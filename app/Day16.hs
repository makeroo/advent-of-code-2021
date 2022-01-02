module Day16 where

import Data.Bits
import Data.List.Split (chunk)
import Debug.Trace (trace)
import Numeric (readHex)

data Quads = Quads Int [Int] deriving (Show)

data Packet = Packet Int Int [Packet] | Constant Int Int deriving (Show)

solve16 contents = r
  where
    bits = parse16 contents
    (packet, _) = trace ("quads: " ++ show bits) parsePacket bits
    versions = packetsVersion [packet]
    r = trace ("packets: " ++ show packet) sum versions

packetsVersion [] = []
packetsVersion ((Constant v _) : rest) = v : packetsVersion rest
packetsVersion ((Packet v _ pp) : rest) = v : packetsVersion (pp ++ rest)

parsePacket :: Quads -> (Packet, Quads)
parsePacket bits = trace ("v: " ++ show v ++ ", t: " ++ show t ++ ", bits2: " ++ show bits2 ++ ", bits3: " ++ show bits3) f v t bits3
  where
    (v, bits2) = readBits 3 bits
    (t, bits3) = readBits 3 bits2
    f v t bits | t == 4 = (Constant v c, bits2)
      where
        (c, bits2) = readConstant bits 0
    f v t bits = (Packet v t pp, bits3)
      where
        (i, bits2) = readBits 1 bits
        (pp, bits3) = readPacketsX i bits2
    readPacketsX 0 bits = (pp, bits3)
      where
        (l, bits2) = readBits 15 bits
        (pp, bits3) = readPackets0 l bits2
    readPacketsX 1 bits = (pp, bits3)
      where
        (l, bits2) = readBits 11 bits
        (pp, bits3) = readPackets1 l bits2
    readPacketsX _ _ = error "cannot happen"

readPackets1 0 bits = ([], bits)
readPackets1 n bits = (p : x, bits3)
  where
    (p, bits2) = parsePacket bits
    (x, bits3) = readPackets1 (n -1) bits2

readPackets0 :: Int -> Quads -> ([Packet], Quads)
readPackets0 0 bits = ([], bits)
readPackets0 l bits = (p : rest, bitsr)
  where
    (p, bits2) = parsePacket bits
    d = quadDiff bits bits2
    (rest, bitsr) = readPackets0 (l - d) bits2

quadDiff (Quads pos1 _) (Quads pos2 _) = pos1 - pos2

readConstant bits b = if i == 0 then (r, bits3) else readConstant bits3 r
  where
    (i, bits2) = readBits 1 bits
    (n, bits3) = readBits 4 bits2
    r = n + b * 16

readBits how_many (Quads pos bits) | how_many <= pos = trace ("how_many: " ++ show how_many ++ ", pos: " ++ show pos ++ ", new_pos: " ++ show new_pos ++ ", r: " ++ show r) (r, Quads new_pos bits)
  where
    new_pos = pos - how_many
    (idx, sub_idx) = new_pos `quotRem` 4
    r = readOn how_many idx sub_idx bits
readBits _ _ = error "end of stream"

readOn :: Int -> Int -> Int -> [Int] -> Int
readOn how_many idx sub_idx bits | how_many + sub_idx <= 4 = trace ("   x: " ++ show x) x
  where
    quad = trace (" how_many: " ++ show how_many ++ ", idx: " ++ show idx ++ ", sub: " ++ show sub_idx) shiftR (bits !! idx) sub_idx
    x = trace ("  quad: " ++ show quad ++ " elem: " ++ show (bits !! idx)) (.&.) quad (shift 1 how_many - 1)
readOn how_many idx sub_idx bits = trace ("   --hi: " ++ show hi ++ ", lo: " ++ show lo) shift hi chunk + lo
  where
    chunk = 4 - sub_idx
    rest = how_many - chunk
    lo = readOn chunk idx sub_idx bits
    hi = readOn rest (idx + 1) 0 bits

parse16 :: String -> Quads
parse16 contents = Quads (4 * length contents) (reverse r)
  where
    r = map parseHex contents
    parseHex :: Char -> Int
    parseHex ch = r
      where
        ((r, _) : _) = readHex [ch]
