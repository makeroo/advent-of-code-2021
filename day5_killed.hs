{-

this version doesn't terminate, it's killed before producing the result

hyp: draw_lines not using tail recursion thus keeping all intermediate
matrix for each point of each line

-}

import System.IO
import Control.Monad
import Data.List.Split
import Debug.Trace

data Line = Line Point Point deriving (Show)
data Point = Point Int Int deriving (Show)
data Matrix = Matrix Int Int [Int] deriving (Show)

main = do
  contents <- readFile "day5.input"

  let lines = parseLines contents
      (w, h) = max_width_and_height 0 0 lines
      matrix = draw_lines (mk_matrix (w+1) (h+1)) [ l | l <- lines, is_ortho l ]

  print ("lines: " ++ show (length lines))
  print ("first line: " ++ show (lines !! 0))
  print ("width: " ++ (show w) ++ ", height: " ++ show h)
  print ("matrix: " ++ show matrix)
  print (count_points matrix (\v -> v > 1))
  

count_points :: Matrix -> (Int -> Bool) -> Int
count_points (Matrix _ _ points) pred = length [x | x <- points, pred x]

draw_lines :: Matrix -> [Line] -> Matrix
draw_lines m [] = m
draw_lines m (l:rest) = draw_lines (draw_line m l) rest -- trace ("rem: "++ show (length rest))

draw_line :: Matrix -> Line -> Matrix
draw_line m (Line (Point x1 y1) (Point x2 y2)) = r
  where
    points = [ (x, y) | x <- range x1 x2, y <- range y1 y2 ] -- trace ("line " ++ show x1 ++ "," ++ show y1 ++ " -> " ++ show x2 ++ "," ++ show y2)
      where
        range x y | x <= y = [x .. y]
        range x y | x > y  = [y .. x]
    r = apply_points m points

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
apply_points (Matrix w h points) ((x, y):rest) = r
  where
    idx = (y * w) + x
    last = w * h - 1
    new_points = (take idx points) ++ [1 + points !! idx] ++ (drop (idx + 1) points) -- trace ("pt: " ++ show x ++ "," ++ show y ++ " on " ++ show idx ++ "/" ++ show last ++ " pts: " ++ show points                       )
    r = apply_points (Matrix w h new_points) rest

apply_points m [] = m

mk_matrix width height = Matrix width height (take (width * height) (repeat 0))

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
