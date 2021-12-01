import System.IO
import Control.Monad

-- https://stackoverflow.com/a/7867786

main = do
  contents <- readFile "day1.input"

  let depths = toInt ( words contents )
      increments = find_increments depths

  print increments

  let triplets = make_triplets ([0,0], depths)

  -- print depths
  -- print triplets

  print (find_increments triplets - 2)

toInt :: [String] -> [Int]
toInt = map read

find_increments :: [Int] -> Int
find_increments (a:b:c) | a>=b = find_increments (b:c)
find_increments (a:b:c) | a<b  = 1 + find_increments (b:c)
find_increments [a,b] | a<b  = 1
find_increments _            = 0


make_triplets :: ([Int], [Int]) -> [Int]
make_triplets ([a,b], f:l) = (a + f) : make_triplets([b+f,f],l)
make_triplets (x, [])        = []
