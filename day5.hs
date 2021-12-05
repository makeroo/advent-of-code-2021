import System.IO
import Control.Monad
import Data.List.Split
import Debug.Trace
import qualified Data.Map as Map

data Line = Line Point Point deriving (Show)
data Point = Point Int Int deriving (Show)


main = do
  contents <- readFile "day5.input"

  let lines = parseLines contents
      (w, h) = max_width_and_height 0 0 lines
      points = draw_lines (w+1) (h+1) [ l | l <- lines, is_ortho l ]
      -- see: https://stackoverflow.com/a/7108655
--λ> :m + Data.Map
--λ> let input = "happy"
      freqs = Map.toList $ Map.fromListWith (+) [(c, 1) | c <- points]
--[('a',1),('h',1),('p',2),('y',1)]

  print ("lines: " ++ show (length lines))
  print ("first line: " ++ show (lines !! 0))
  print ("width: " ++ (show w) ++ ", height: " ++ show h)
  print ("points: " ++ show (length points))
  print (length [ x | (x, v) <- freqs, v > 1 ])
  


draw_lines :: Int -> Int -> [Line] -> [Int]
draw_lines w h [] = []
draw_lines w h (l:rest) = (draw_line w h l) ++ draw_lines w h rest -- trace ("rem: "++ show (length rest))

draw_line :: Int -> Int -> Line -> [Int]
draw_line w h (Line (Point x1 y1) (Point x2 y2)) = points
  where
    points = [ y * w + x | x <- range x1 x2, y <- range y1 y2 ] -- trace ("line " ++ show x1 ++ "," ++ show y1 ++ " -> " ++ show x2 ++ "," ++ show y2)
      where
        range x y | x <= y = [x .. y]
        range x y | x > y  = [y .. x]

{-
      
0,0,0,
0,0,0,
0,1,1,
0,0,0

w = 3, h = 2
last = 5
Point 1 1 -> idx = 3 + 1 = 4
 0 0 0
 0 . 0
prev = take 4 + (1+!!4) + drop 5

    --  if idx == 0 then (1 + points !! 0):drop 1 points else
    -- if idx == (w - 1) * (h - 1) then (take last points) ++ [1 + points !! last] else
                   
-}


is_ortho (Line (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

max_width_and_height :: Int -> Int -> [Line] -> (Int, Int)
max_width_and_height w h ((Line (Point x1 y1) (Point x2 y2)):rest) = max_width_and_height (maximum [w, x1, x2]) (maximum [h, y1, y2]) rest
max_width_and_height w h [] = (w, h)

parseLines :: String -> [Line]
parseLines contents = catMaybes [ parseLine line | line <- text_lines ]
  where
    text_lines = splitOn "\n" contents

parseLine :: String -> Maybe Line
parseLine text_line = line
  where
    w = words text_line
    line = if length w == 3 then Just (Line (parsePoint (w !! 0)) (parsePoint (w !! 2))) else Nothing

parsePoint :: String -> Point
parsePoint text = Point x y
  where
    (x:y:[]) = row_numbers text

-- https://stackoverflow.com/a/40327860
catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

row_numbers str = toInt (splitOn "," str)
                 
toInt :: [String] -> [Int]
toInt = map read
