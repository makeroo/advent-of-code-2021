import Data.Bits (Bits (xor))
import Data.Char (ord)
import Data.List (elem, sortBy)
--import Debug.Trace (trace)
import System.Environment (getArgs)

trace a b = b

main = do
  args <- getArgs

  contents <- readFile $ head args

  let r = solve15 contents

  print r

data Map = Map Int Int [String] deriving (Show, Eq)

data Point = Point Int Int deriving (Show, Eq)

data Path = Path Int [Point] deriving (Show)

maxElem = ord '9'

minElem = ord '1'

solve15 contents = r
  where
    m = parse15 contents
    max_const = maxCost m
    paths = trace ("map: " ++ show m) findPaths2 max_const [Path 0 [Point 0 0]] m []
    costs = map pathCost paths
    r = trace ("sol: " ++ show (head paths)) head costs

pathCost (Path c _) = c

costAt (Point x y) (Map _ _ ll) = ord ((ll !! y) !! x) - ord '0'

maxCost (Map w h ll) = w * h * m + 1
  where
    m = ord $ maximum $ map maximum ll

adjacents = [(-1, 0), (1, 0), (0, -1), (0, 1)]

findPaths2 :: Int -> [Path] -> Map -> [Path] -> [Path]
findPaths2 _ [] _ r = r
--findPaths2 max_cost (path@(Path cost (pt@(Point x y) : prest)) : rest) m@(Map w h ll) r | x < 0 = findPaths2 max_cost rest m r -- out of bounds
--findPaths2 max_cost (path@(Path cost (pt@(Point x y) : prest)) : rest) m@(Map w h ll) r | x >= w = findPaths2 max_cost rest m r -- out of bounds
--findPaths2 max_cost (path@(Path cost (pt@(Point x y) : prest)) : rest) m@(Map w h ll) r | y < 0 = findPaths2 max_cost rest m r -- out of bounds
--findPaths2 max_cost (path@(Path cost (pt@(Point x y) : prest)) : rest) m@(Map w h ll) r | y >= h = findPaths2 max_cost rest m r -- out of bounds
findPaths2 max_cost (path@(Path cost (pt@(Point x y) : prest)) : rest) m@(Map w h ll) r | pt `elem` prest = findPaths2 max_cost rest m r -- dead end
findPaths2 max_cost (path@(Path cost (pt@(Point x y) : prest)) : rest) m@(Map w h ll) r | x == w -1 && y == h -1 && cost > max_cost = findPaths2 max_cost rest m r -- found but ignored!
findPaths2 max_cost (path@(Path cost (pt@(Point x y) : prest)) : rest) m@(Map w h ll) r | x == w -1 && y == h -1 = trace ("found: " ++ show path) findPaths2 cost rest m [path] -- found!
findPaths2 max_cost (path@(Path cost (pt@(Point x y) : prest)) : rest) m@(Map w h ll) r | (w -1 - x) + h -1 - y + cost > max_cost = trace ("too costly: " ++ show path ++ ", pt:" ++ show x ++ "," ++ show y ++ ", " ++ show max_cost) findPaths2 max_cost rest m r
findPaths2 max_cost (path@(Path cost pp@(pt@(Point x y) : prest)) : rest) m@(Map w h ll) r = findPaths2 max_cost (new_paths ++ rest) m r
  where
    c = costAt pt m
    next_pos = [Point (x + dx) (y + dy) | (dx, dy) <- adjacents, validPt (x + dx) (y + dy) w h]
    sorted_next_pos = sortBy (\pt1 pt2 -> (costAt pt1 m) `compare` (costAt pt2 m)) next_pos
    new_paths = [Path (c + cost) (pt : pp) | pt <- sorted_next_pos]
findPaths2 _ ((Path _ []) : rest) _ _ = error "empty path"

validPt x y w h = x >= 0 && x < w && y >= 0 && y < h

{-
findPaths2 max_cost (p : rest) m r = findPaths2 new_max_cost (next_paths ++ rest) m r
  where
    x = 0
-}

{-
findPaths :: Int -> Path -> [Point] -> Map -> [Path]
findPaths max_cost (Path _ p@(pt : rest)) done m | pt `elem` done = [] --  trace ("dead end: " ++ show p) []
findPaths max_cost (Path _ path@(pt@(Point x y) : rest)) _ (Map w h _) | x < 0 || x >= w || y < 0 || y >= h = [] -- out of map
findPaths max_cost (Path c path@(pt@(Point x y) : rest)) _ m@(Map w h ll) | c + minElem * ((w - 1 - x) + (h - 1 - y)) > max_cost = [] -- too costly
findPaths max_cost path@(Path _ (pt@(Point x y) : rest)) done (Map w h _) | x == w - 1 && y == h - 1 = trace ("found: " ++ show path) [path] -- found path

findPaths max_cost (Path _ path@(pt@(Point x y) : rest)) done m@(Map w h _) = concat [findPaths max_cost (Point (x + dx) (y + dy) : path) (pt : done) m | (dx, dy) <- adjacents, x + dx >= 0 && x + dy < w && y + dy >= 0 && y + dy < h]
  where
    nextPos = [Point (x + dx) (y + dy) | (dx, dy) <- adjacents, x + dx >= 0 && x + dy < w && y + dy >= 0 && y + dy < h]
    r = doNext [] nextPos max_cost path done m
    doNext r [] _ _ _ _ = r
    doNext pt
findPaths _ (Path _ []) _ _ = error "empty path"
-}

parse15 contents = Map w h ll
  where
    ll = lines contents
    w = length $ head ll
    h = length ll
