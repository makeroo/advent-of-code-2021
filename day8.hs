import System.IO
import Control.Monad
import Data.List.Split
import Data.Char
import Debug.Trace
import qualified Data.Map as Map
import Data.List
import Data.Ord


{-
0 abcefg  6
1 cf      2 *
2 acdeg   5
3 acdfg   5
4 bcdf    4 *
5 abdfg   5
6 abdefg  6
7 acf     3 *
8 abcdefg 7
9 abcdfg  6
-}

main = do
  contents <- readFile "day8.input"

  let signals = parse_signals contents

  print signals
  print (length [ x | (_,o) <- signals, x <- o, length x < 5 || length x == 7 ])

parse_signals contents = signals
  where
    lines = splitOn "\n" contents
    patterns_and_outputs = [ map strip (splitOn "|" line) | line <- lines ]
    signals = [ (splitOn " " (strip p), splitOn " " (strip (o!!0))) | p:o <-  patterns_and_outputs, length o > 0 ]

strip (x:rest) | isSpace x = strip rest
strip (x:rest) = x : reverse (stripr (reverse rest))
  where
    stripr (x:rest) | isSpace x = stripr rest
    stripr x = x
strip [] = []

-- subarray f t array = take (t - f) (drop f array)


                 
-- toInt :: [String] -> [Int]
-- toInt = map read

-- https://newbedev.com/finding-index-of-element-in-a-list-in-haskell
-- minIndex ::  Ord a => [a] -> Int
-- minIndex = fst . minimumBy (comparing snd) . zip [0..]
