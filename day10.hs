import System.IO
import Control.Monad
import qualified Data.Map as Map
import Data.List
import Debug.Trace

main = do
  contents <- readFile "day10.input"

  let patterns = lines contents

  -- print patterns
  -- print matching_states
  print ("1st star: " ++ show (first_star patterns))
  print ("2nd star: " ++ show (second_star patterns))

second_star patterns = res
  where
    check = map (check_syntax []) patterns
    only_incompletes = trace (show check) [ s | (s, x) <- check, x == Nothing]
    costs = trace (show only_incompletes) sort (map (weight 0) only_incompletes)
    l = length costs
    res = costs !! (quot l 2)
    

weight x [] = x
weight x (a:rest) = weight (5 * x + v) rest
  where
    f (Just x) = x
    v = 1 + f (elemIndex a open)

first_star patterns = sum check
  where
    check = map (\(s, x) -> error_value x) (map (check_syntax []) patterns)
      where
        error_value (Just x) = errors Map.! x
        error_value _ = 0

open = "([{<"
close = ")]}>"
error_values = [3, 57, 1197, 25137]

matching_states = Map.fromList (zip open close)
errors = Map.fromList (zip close error_values)

check_syntax x [] = (x, Nothing)
check_syntax [] (x:rest) = check_syntax [x] rest
check_syntax s (x:rest) | x `elem` open = check_syntax (x:s) rest
check_syntax (s:srest) (x:rest) | matching s x = check_syntax srest rest
check_syntax s (x:rest) = (s, Just x)

matching a b = Map.lookup a matching_states == Just b
