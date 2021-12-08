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
8 abcdefg 7 *
9 abcdfg  6

8:acedgfb 235:cdfbe 235:gcdfa 235:fbcad 7:dab 235:cefabd 235:cdfgeb 4:eafb 235:cagedb 1:ab |
235:cdfeb 235:fcadb 235:cdfeb 235:cdbaf


a -> cf         
b -> cf         
c ->   BDEG
d ->            acf  !a
e ->     bcdf  bd
f ->     bcdf  bd
g ->

abdef cde

The string ab means a can be either c or f because:
1. length 2 matches "cf"
2. each char in the pattern intersect with "cf"

so:
take all the patterns whose length matches the filtering one
take their union
for each letter of the filtering pattern
   intersect their value with the union
-}

type Guess = Map.Map Char String

main = do
  contents <- readFile "day8.input"

  let signals = parse_signals contents

  print signals
  print (length [ x | (_,o) <- signals, x <- o, length x < 5 || length x == 7 ])
  -- print (signals !! 0)
  print (apply_guess "dab" (apply_guess "ab" initial_guess))
  print (nub [length f | (f,s) <- signals])
  print (nub [length s | (f,s) <- signals])

  let flat_lines = [ f ++ s | (f, s) <- signals]
      solutions = [ solve_line line | line <- flat_lines ]

  {-
  let flat_lines = [ f ++ s | (f, s) <- signals]
      fg = first_guess (flat_lines!!0) initial_guess
      sorted_fg = sortOn (\(_, b) -> length b) (Map.toList fg)
      combs = combinations "" sorted_fg

  print ( fg )
  -- print ( [ fg Map.! x | x <- "abgdefg" ])
  print ( sorted_fg )
  print ( combs )
  --print ( [ valid_pattern (combs!!0) p | p <- flat_lines!!0 ])
  --print ( [ (comb, [valid_pattern comb p | p <- flat_lines!!0 ]) | comb <- combs ] )
  -- print (  [valid_pattern comb p | p <- flat_lines!!0, comb <- combs ] )
  --print ( [ [valid_pattern comb p | p <- flat_lines!!0 ] | comb <- combs ] )
  --print (filter (\x -> all (\y -> length y > 0) x) [ [valid_pattern comb p | p <- flat_lines!!0 ] | comb <- combs ] )
  -- sbagliato print (  [valid_pattern comb p | p <- flat_lines!!0, comb <- combs ] )
-}
  -- print ( solve_line (flat_lines!!0) )

  -- print ( [drop 10 (s!!0) | s <- solutions])
  print (sum solutions)
  
--solve_circuit :: ([String], [String]) -> String
--solve_circuit (a,b) = solve_line (a ++ b)

solve_line :: [String] -> Int
solve_line line = res
  where
    fg = first_guess line initial_guess
    sorted_fg = sortOn (\(_, b) -> length b) (Map.toList fg)
    combs = combinations "" sorted_fg
    valid_patts = filter (\x -> all (\y -> length y > 0) x) [ [valid_pattern comb p | p <- line] | comb <- combs ]
    digits = drop 10 (valid_patts !! 0)
    res = sum (map (\(p, [v]) -> v * 10 ^ p) (zip [0..] (reverse digits)))

valid_pattern :: [(Char, Char)] -> String -> [Int]
valid_pattern p chk = elemIndices (sort tp) first_filter
  where
    tp = [ x | (l, x) <- p, l `elem` chk ]

combinations :: String -> [(Char, String)] -> [[(Char,Char)]]
combinations binded [(c,vv)] = [[(c,x) | x <- vv \\ binded]]
combinations binded ((c,vv):rest) = [(c,x):y | x <- vv \\ binded, y <- combinations (x:binded) rest]



first_guess :: [String] -> Guess -> Guess
first_guess (m:rest) guess = first_guess rest (apply_guess m guess)
first_guess [] guess = guess

apply_guess :: String -> Guess -> Guess
apply_guess x guess = next_guess x guess
  where
    l = length x
    pp = unionx (filter (\a -> length a == l) first_filter)
    --adjustments = [ (v, res (Map.lookup v guess)) | v <- x ]
    --  where
    --    res (Just e) = intersect pp e
    --next_guess ((k, v):rest) g = next_guess rest Map.adjust (\t -> v) k g
    --next_guess [] g = g

    next_guess :: String -> Guess -> Guess
    next_guess (c:rest) g = next_guess rest (Map.adjust (\v -> intersect pp v) c g)
    next_guess [] g = g

    --ppm = Map.fromListWith (++) pp
    --t = [ v | 
    --next_guess = [ filter_with p (Map.lookup idx ppm) | (p, idx) <- zip guess [1..]]
    --  where
    --    filter_with p (Just x) = intersect p x
    --    filter_with p _ = p

unionx :: [String] -> String
unionx [] = ""
unionx [a] = a
unionx (a:b:rest) = unionx ((union a b):rest)

reduce2 :: (a -> a -> a) -> [a] -> Maybe a
reduce2 _ [] = Nothing
reduce2 _ [a] = Just a
reduce2 f (a:b:rest) = reduce2 f ((f a b):rest)

initial_guess = Map.fromList (zip "abcdefg" (repeat "abcdefg"))
first_filter = [
  "abcefg",  -- 0
  "cf",      -- 1
  "acdeg",   -- 2
  "acdfg",   -- 3
  "bcdf",    -- 4
  "abdfg",   -- 5
  "abdefg",  -- 6
  "acf",     -- 7
  "abcdefg", -- 8
  "abcdfg"   -- 9
  ]

parse_signals contents = signals
  where
    lines = splitOn "\n" contents
    patterns_and_outputs = [ map strip (splitOn "|" line) | line <- lines ]
    signals = [ (map sort (splitOn " " (strip p)), map sort (splitOn " " (strip (o!!0)))) | p:o <-  patterns_and_outputs, length o > 0 ]

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
