-- import System.IO
-- import Control.Monad
import System.Environment (getArgs)
import Data.List.Split
import Data.List
import Debug.Trace
import Data.Char

data Fold = AlongX | AlongY deriving (Show, Eq)

data Matrix = Matrix Int Int [Int] deriving (Show)


main = do
  args <- getArgs
  contents <- readFile $ head args

  let (points, fold_cmds) = parse_fold_cmds contents
      r = fold_paper fold_cmds points
      r2 = filter_nothing $ map (fold_paper1 $ fold_cmds !! 0) points
      -- m = draw_matrix r

  --print points
  -- print $ fold_cmds !! 0
  -- print $ sort $ nub r2
  print ( "star 1: "++ show ( length $ nub r2))

  print $ nub r

  let (w2, h2) = max_width_and_height 0 0 r
      w = w2+1
      h = h2 +1
      -- matrix = draw_lines (mk_matrix (w+1) (h+1)) [ l | l <- lines, is_ortho l ]

  print (w, h)

  let m = draw_matrix (w, h, replicate (h * w) 0) r

  print (w, h, m)

  let mshow = map (\v -> chr(46 -11*v)) m
      mshow2 = chunksOf w mshow

  print mshow2
  putStr $ unlines mshow2

--splitEvery :: Int -> [a] -> [[a]]
--splitEvery x v | length v > x = (take x v) : (splitEvery x (drop x v))
--splitEvery x v = [v]

draw_matrix:: (Int,Int,[Int]) -> [(Int,Int)] -> [Int]
draw_matrix (w,h,m) [] = m
draw_matrix (w,h,m) ((x,y):rest) = draw_matrix (w,h,pre ++ [1] ++ post) rest
  where
    idx = y*w+x
    pre = take idx m
    post = drop (idx+1) m

max_width_and_height :: Int -> Int -> [(Int,Int)] -> (Int, Int)
max_width_and_height w h ((x, y):rest) = max_width_and_height (maximum [w, x]) (maximum [h, y]) rest
max_width_and_height w h [] = (w, h)

{-
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
-}

fold_paper :: [(Fold, Int)] -> [(Int,Int)] -> [(Int,Int)]
fold_paper (cmd:rest) points =  fold_paper rest $ filter_nothing $ map (fold_paper1 cmd) points
fold_paper [] points = points

fold_paper1 :: (Fold, Int) -> (Int, Int) -> Maybe (Int, Int)
fold_paper1 (AlongX, v) p@(x, y) | x < v = Just p
fold_paper1 (AlongX, v) (x, y) | x > v = Just (2 * v - x, y)  
fold_paper1 (AlongX, v) (x, _) | v == x = Nothing
fold_paper1 (AlongY, v) p@(x, y) | y < v = Just p  
fold_paper1 (AlongY, v) (x, y) | y > v = Just (x, 2 * v - y)  
fold_paper1 (AlongY, v) (x, y) | y == v = Nothing

filter_nothing :: [Maybe a] -> [a]
filter_nothing [] = []
filter_nothing (Nothing:rest) = filter_nothing rest
filter_nothing (Just x:rest) = x:filter_nothing rest

parse_fold_cmds :: String -> ([(Int, Int)], [(Fold, Int)])
parse_fold_cmds contents = r
  where
    cmds = lines contents
    (points, folds) = split_points [] cmds
    split_points x ("":rest) = (x, parse_folds rest)
    split_points x (v:rest) = split_points ((point $ toInt $ splitOn "," v):x) rest
    point (x:y:[]) = (x,y) 
    parse_folds (x:rest) | isPrefixOf pfx x = parse_fold x:parse_folds rest
    parse_folds (_:rest) = parse_folds rest
    parse_folds [] = []
    parse_fold x = v (splitOn "=" (drop (length pfx) x))
      where
        v (a:b:[]) = (f a, strToInt b)
        f "x" = AlongX
        f "y" = AlongY
    r = (points, folds)
    pfx = "fold along "


toInt :: [String] -> [Int]
toInt = map read

strToInt :: String -> Int
strToInt = read
