import System.IO
import Control.Monad
import Data.Bits
import Data.Char  (digitToInt)
import Data.Maybe (listToMaybe)
import Numeric    (readInt)

main = do
  contents <- readFile "day3.input"

  let digit_strings = words contents
      digit_string_length = length (digit_strings !! 0)
      digit_strings_count = length digit_strings
      values = onlyInts ( map readBin digit_strings )
      ones_count  = count_ones (take digit_string_length zeros) values
      gamma_rate_string = calc_gamma_rate digit_strings_count (reverse ones_count)
      gamma_rate = bit_string_to_number gamma_rate_string
      total = 2 ^ digit_string_length - 1
      epsilon_rate = total - gamma_rate
      

  print digit_strings_count
  -- print digit_strings
  -- print values
  -- print (map (add_ones [0,0,0,0,0,0,0,0,0,0,0,0]) values)
  print ones_count
  print gamma_rate
  print total
  print epsilon_rate
  print (gamma_rate * epsilon_rate)



zeros = 0 : zeros

powers_of_2 = iterate (*2) 1

count_ones:: [Int] -> [Int] -> [Int]
count_ones p (v:rest) = count_ones (add_ones p v) rest
count_ones p []     = p

bit_and (a, b) = if a .&. b > 0 then 1 else 0

tuple_sum (a, b) = a + b
tuple_prod (a, b) = a * b

add_ones:: [Int] -> Int -> [Int]
add_ones p v = x
   where
     chk_list = zip powers_of_2 (repeat v)
     chk_res = map bit_and chk_list
     t = zip p chk_res
     x = map tuple_sum t

calc_gamma_rate:: Int -> [Int] -> [Int]
calc_gamma_rate total (v:rest) = (if v > limit then 1 else 0) : calc_gamma_rate total rest
  where
    limit = quot total 2
calc_gamma_rate total [] = []

bit_string_to_number:: [Int] -> Int
bit_string_to_number x = y
  where
    t = reverse x
    vv = zip powers_of_2 t
    t2 = map tuple_prod vv
    y = sum t2


readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt
-- readBin "1001" == Just 9

onlyInts :: Integral a => [Maybe a] -> [a]
onlyInts (Just a:rest) = a : onlyInts rest
onlyInts (_:rest)      = onlyInts rest
onlyInts []            = []
