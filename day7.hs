import System.IO
import Control.Monad
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map
import Data.List
import Data.Ord

data Cost = Cost Int Int

main = do
  contents <- readFile "day7.input"

  let crabs = toInt ( splitOn "," contents )
      freqs = Map.fromListWith (+) [(c, 1) | c <- crabs]
      crabs_array = [ def0 (Map.lookup x freqs) | x <- [0..(maximum (Map.keys freqs))] ]
        where
          def0 (Just x) = x
          def0 _        = 0
      fuel_0 = fuel_for 0 crabs_array
      fuel_a = fuel_all (Cost 0 0) (Cost fuel_0 (sum (drop 1 crabs_array))) crabs_array 0
      fuel_2x = fuelx_for 2 crabs_array
      fuel_5x = fuelx_for 5 crabs_array

  print crabs
  print freqs
  print crabs_array
  print fuel_0
  print (fuel_for 2 crabs_array)
  print (fuel_for 1 crabs_array)
  print (fuel_for 3 crabs_array)
  print fuel_a
  print (minIndex fuel_a)
  print fuel_2x
  print fuel_5x
  print (minimum [fuelx_for p crabs_array | p <- [0..length crabs_array]])

fuel_all :: Cost -> Cost -> [Int] -> Int -> [Int]
fuel_all _ _ x p | p == length x = []
fuel_all (Cost left_fuel left_crabs) (Cost right_fuel right_crabs) array curr_pos = next_cost:(fuel_all next_left next_right array next_pos)
  -- trace ("p:"++show curr_pos++" "++show left_fuel ++"-"++show left_crabs++" .. "++show right_fuel ++ "-"++show right_crabs++ " T:"++show(left_crabs+right_crabs+curr_crabs) ++ " F:"++ show(left_fuel+right_fuel))
  where
    next_pos = curr_pos + 1
    curr_crabs = array !! curr_pos
    next_crabs = array !! next_pos
    next_left_crabs = left_crabs + curr_crabs
    next_right_crabs = right_crabs - next_crabs
    next_left_fuel = left_fuel + left_crabs + curr_crabs
    next_right_fuel = right_fuel - right_crabs
    next_left = Cost next_left_fuel next_left_crabs
    next_right = Cost next_right_fuel next_right_crabs
    next_cost = left_fuel + right_fuel

fuel_for :: Int -> [Int] -> Int
fuel_for pos array = cost_upto (pos-1) + cost_from (pos+1)
  where
    l = length array
    cost_upto x | x < 0 = 0
    cost_upto x         = sum costs
      where
        crabs = subarray 0 (x+1) array
        cost = reverse [1 .. x + 1 ]
        costs = map (\(x,y) -> x * y) (zip crabs cost)
    cost_from x = sum costs
      where
        crabs = subarray x l array
        cost = [1 .. l - x]
        costs = map (\(x,y) -> x * y) (zip crabs cost)

fuelx_for :: Int -> [Int] -> Int
fuelx_for pos array = cost_upto (pos-1) + cost_from (pos+1)
  where
    l = length array
    cost_upto x | x < 0 = 0
    cost_upto x         = sum costs
      where
        crabs = subarray 0 (x+1) array
        cost = reverse [1 .. x + 1 ]
        costs = map (\(x,y) -> x * gauss(y)) (zip crabs cost)
    cost_from x = sum costs
      where
        crabs = subarray x l array
        cost = [1 .. l - x]
        costs = map (\(x,y) -> x * gauss(y)) (zip crabs cost)

gauss x = quot (x * (x + 1)) 2

subarray f t array = take (t - f) (drop f array)


                 
toInt :: [String] -> [Int]
toInt = map read

-- https://newbedev.com/finding-index-of-element-in-a-list-in-haskell
minIndex ::  Ord a => [a] -> Int
minIndex = fst . minimumBy (comparing snd) . zip [0..]
