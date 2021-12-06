import System.IO
import Control.Monad
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map


main = do
  contents <- readFile "day6.input"

  let cycles = toInt ( splitOn "," contents )
      freqs = Map.fromListWith (+) [(c, 1) | c <- cycles]
      pop = [ def0 (Map.lookup x freqs) | x <- [0..8] ]
        where
          def0 (Just x) = x
          def0 _        = 0
      pop1 = age pop
      pop2 = age pop1
--      how_many = [ v * (length (age_nday_many 80 [a])) | (a, v) <- freqs ]
--      how_many256 = [ v * (length (age_nday_many 256 [a])) | (a, v) <- freqs ]

  print cycles
  print freqs
  print pop
  --print pop1
  --print pop2
  print (sum (agen 80 pop))
  print (sum (agen 256 pop))
--  print how_many
--  print (sum how_many)
--  print (sum how_many256)

{-


-}

--012345678
--123456a8b
-- a = 0 + 7
-- b = 0

agen 0 x = x
agen g x = agen (g - 1) (age x)
  
age :: [Int] -> [Int]
age pop = trace ( "r:" ++ show r) r
  --trace ("o1:" ++ show o1 ++ ", o2:" ++ show o2) o1 ++ [new] ++ o2 ++ [pop !! 0]
  where
    o1 = subarray 1 7 pop
    new = (pop !! 7) + (pop !! 0)
    o2 = [pop !! 8]
    r = o1 ++ [new] ++ o2 ++ [pop !! 0]

subarray f t array = take (t - f) (drop f array)


                 
toInt :: [String] -> [Int]
toInt = map read
