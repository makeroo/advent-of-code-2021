-- import System.IO
-- import Control.Monad
import System.Environment (getArgs)
import Data.List.Split
import Data.List
import Debug.Trace

data Fold = AlongX | AlongY deriving (Show, Eq)

main = do
  args <- getArgs
  contents <- readFile $ head args

  let (points, fold_cmds) = parse_fold_cmds contents
      -- r = fold_paper fold_cmds points
      r2 = filter_nothing $ map (fold_paper1 $ fold_cmds !! 0) points

  --print points
  -- print $ fold_cmds !! 0
  -- print $ sort $ nub r2
  print ( "star 1: "++ show ( length $ nub r2))

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
