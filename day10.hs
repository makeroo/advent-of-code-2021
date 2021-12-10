import System.IO
import Control.Monad
import qualified Data.Map as Map

main = do
  contents <- readFile "day10.input"

  let patterns = lines contents

  -- print patterns
  -- print matching_states
  print ("1st star: " ++ show (sum (map (check_syntax []) patterns)))

open = "([{<"
close = ")]}>"
error_values = [3, 57, 1197, 25137]

matching_states = Map.fromList (zip open close)
errors = Map.fromList (zip close error_values)

check_syntax _ [] = 0
check_syntax [] (x:rest) = check_syntax [x] rest
check_syntax s (x:rest) | x `elem` open = check_syntax (x:s) rest
check_syntax (s:srest) (x:rest) | matching s x = check_syntax srest rest
check_syntax _ (x:rest) = errors Map.! x

matching a b = Map.lookup a matching_states == Just b
