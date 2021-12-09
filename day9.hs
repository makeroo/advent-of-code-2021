import System.IO
import Control.Monad
import Data.List.Split
import Data.Char
import Debug.Trace
import qualified Data.Map as Map
import Data.List
import Data.Ord

data HeightMap = HeightMap Int Int [String]

main = do
  contents <- readFile "day9.input"

  let in_lines = lines contents
      width = length (in_lines !! 0)
      height = length in_lines
      height_map = HeightMap width height in_lines

  print (sum (filter (\x -> x > 0) [ 1 + is_minimum x  y height_map | x <- [0..width - 1], y <- [0..height - 1] ]))

neighbours = [ (0,-1), (-1,0), (1,0), (0,1) ]

is_minimum x y (HeightMap w h heights) = if (and [ height_at x y < height_at nx ny | (nx, ny) <- neb_pos ]) then v else -1
  where 
    height_at x y = ord ((heights !! y) !! x) - (ord '0')
    v = height_at x y
    neb_pos = [ (dx + x, dy + y) | (dx,dy) <- neighbours, x + dx >= 0 && x + dx < w && y + dy >= 0 && y + dy < h]

reduce2 :: (a -> a -> a) -> [a] -> Maybe a
reduce2 _ [] = Nothing
reduce2 _ [a] = Just a
reduce2 f (a:b:rest) = reduce2 f ((f a b):rest)


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
