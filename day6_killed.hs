import System.IO
import Control.Monad
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map

data Line = Line Point Point deriving (Show)
data Point = Point Int Int deriving (Show)


main = do
  contents <- readFile "day6.input"

  let cycles = toInt ( splitOn "," contents )
      -- iter1_cycles = age_1day_many cycles
      r = age_nday_many 80 cycles

  -- print cycles
  -- print iter1_cycles
  print (length r)

age_1day :: Int -> Int
age_1day x = if x > 0 then x - 1 else 6

age_1day_many xx = yy
  where
    temp = map age_1day xx
    new_born = length (filter (\a -> a == 6) temp)
    yy = temp ++ (take new_born (repeat 8))

age_nday_many 0 xx = xx
age_nday_many n xx = age_nday_many (n - 1) (age_1day_many xx)

                 
toInt :: [String] -> [Int]
toInt = map read
