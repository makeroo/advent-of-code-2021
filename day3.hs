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
      

  -- print digit_strings_count
  -- print digit_strings
  -- print values
  -- print (map (add_ones [0,0,0,0,0,0,0,0,0,0,0,0]) values)
  -- print ones_count
  -- print gamma_rate
  -- print total
  -- print epsilon_rate
  print (gamma_rate * epsilon_rate)

  let oxy_rate = filter_rate (2 ^ 11) 1 values
      co2_rate = filter_rate (2 ^ 11) 0 values

  print oxy_rate
  print co2_rate
  print (oxy_rate * co2_rate)


filter_rate :: Int -> Int -> [Int] -> Int
filter_rate _ _ [x] = x
filter_rate bit chk (x:xs) | bit > 0 = v
  where
    set_1 = [x | x <- x:xs, x .&. bit > 0]
    set_0 = [x | x <- x:xs, x .&. bit == 0]
    sub_set = if (chk == 1 && (length set_1 >= length set_0)) ||
                 (chk == 0 && (length set_1 < length set_0)) then set_1 else set_0
    v = filter_rate (quot bit 2) chk sub_set


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


-- https://stackoverflow.com/a/5922212
readBin :: Integral a => String -> Maybe a
readBin = fmap fst . listToMaybe . readInt 2 (`elem` "01") digitToInt

onlyInts :: Integral a => [Maybe a] -> [a]
onlyInts (Just a:rest) = a : onlyInts rest
onlyInts (_:rest)      = onlyInts rest
onlyInts []            = []
