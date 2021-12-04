import System.IO
import Control.Monad
import Data.List.Split
import Debug.Trace

data NumberState = Nop | Extracted deriving (Eq, Show)

-- https://stackoverflow.com/a/7867786

-- improvements:
-- 1. define table as data type with it own operations (eg. apply number, is winning...)
--    eg. instead of couples num/state, a 5*5 matrix + 5+5 rows/cols matched
--        when a number is matched, increase its row and col count so
--        determining if a table is complete is o(1)

main = do
  contents <- readFile "day4.input"

  let lines = splitOn "\n" contents
      extraction = lines !! 0
      extracted = row_numbers extraction
      table_lines = drop 1 lines
      tables = parse_tables table_lines

  {-
      -- fst = extracted !! 0
      -- ttry = apply_number fst (tables !! 0)
  
  -- print lines
  -- print extraction
  print extracted
  print (length tables)
  print [ t | t <- tables, length t /= 25 ]
  print (tables !! 0)
  print (traspose (tables !! 0))
  --print (length (tables !! 0))

  print (fst, ttry)
  print (is_winning_table [
            (3,Nop),(82,Nop),(18,Nop),(50,Nop),(90,Nop),
            (30,Extracted),(54,Extracted),(80,Extracted),(11,Extracted),(10,Extracted),
            (16,Nop),(37,Nop),(52,Nop),(67,Nop),(28,Nop),
            (60,Nop),(79,Nop),(7,Nop),(65,Nop),(58,Nop),
            (76,Nop),(83,Nop),(38,Nop),(51,Nop),(1,Nop)])
  -}
  print (play_bingo extracted tables)

  print (play_bingo_last_table extracted 0 tables [])

play_bingo_last_table :: [Int] -> Int -> [[(Int, NumberState)]] -> [[(Int, NumberState)]] -> Int

play_bingo_last_table _ last_extr [] [t] = trace ("final "++(show last_extr) ++ ":"++ show t) calc_result last_extr [t]
play_bingo_last_table (extraction:rest) _ tables _ = r
  where
    new_tables = trace ("extracted " ++ show extraction) [ apply_number extraction t | t <- tables ]
    (winning_tables, non_winning_tables) = split_winners [] [] new_tables
    r = trace ("remaining tables " ++ show(length non_winning_tables)) play_bingo_last_table rest extraction non_winning_tables winning_tables
-- play_bingo_last_table a b c = trace ("boh " ++ (show a) ++ ", " ++ (show b) ++ ", " ++ (show c)) 0-1

split_winners :: [[(Int, NumberState)]] -> [[(Int, NumberState)]] -> [[(Int, NumberState)]] -> ([[(Int, NumberState)]], [[(Int, NumberState)]])
split_winners a b []                            = (a, b)
split_winners a b (t:rest) | is_winning_table t = split_winners (a++[t]) b     rest 
split_winners a b (t:rest)                      = split_winners a     (b++[t]) rest


play_bingo :: [Int] -> [[(Int, NumberState)]] -> Int
play_bingo (extraction:rest) tables = wt
  where
    new_tables = trace ("extracted " ++ show extraction) [ apply_number extraction t | t <- tables ]
    winning_tables = check_winners new_tables
    wt = if length winning_tables > 0 then calc_result extraction winning_tables else play_bingo rest new_tables

calc_result :: Int -> [[(Int, NumberState)]] -> Int
calc_result extraction [t] = r
  where
    non_extracted = [ n | (n, s) <- t, s == Nop ]
    r = extraction * sum non_extracted

apply_number :: Int -> [(Int, NumberState)] -> [(Int, NumberState)]
apply_number n table = [ (x, if n == x then Extracted else orig) | (x, orig) <- table ]

check_winners tables = [ t | t <- tables, is_winning_table t ]

is_winning_table table = res
  where
    only_states = [ s | (n, s) <- table ]
    trasposed = traspose only_states
    res = winning only_states || winning trasposed

winning (s1:s2:s3:s4:s5:rest) = r
  where
    r = (s1 == Extracted && s2 == Extracted && s3 == Extracted && s4 == Extracted && s5 == Extracted) || winning rest
winning [] = False

traspose t = r
  where
    l1 = take_every 0 5 t
    l2 = take_every 1 5 t
    l3 = take_every 2 5 t
    l4 = take_every 3 5 t
    l5 = take_every 4 5 t
    r = l1 ++ l2 ++ l3 ++ l4 ++ l5

take_every from step numbers = [ numbers !! pos | pos <- positions from ]
  where
    l = length numbers
    positions f = if f < l then f:positions (f+step) else []

parse_tables :: [String] -> [[(Int,NumberState)]]
parse_tables ("":l1:l2:l3:l4:l5:rest) = [a_table] ++ parse_tables rest
  where
    row1 = table_numbers l1
    row2 = table_numbers l2
    row3 = table_numbers l3
    row4 = table_numbers l4
    row5 = table_numbers l5
    a_table = row1 ++ row2 ++ row3 ++ row4 ++ row5

parse_tables _ = []

table_numbers line = zip (toInt [str | str <- splitOn " " line, length str > 0 ]) (repeat Nop)
    

-- row_numbers :: String -> [Int]
row_numbers str = toInt (splitOn "," str)
                 
toInt :: [String] -> [Int]
toInt = map read
