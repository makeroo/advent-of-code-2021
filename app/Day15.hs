module Day15 where

import Data.Char (ord)
import Data.List (elem)
import Debug.Trace (trace)

data Map = Map Int Int [String] deriving (Show, Eq)

data Point = Point Int Int deriving (Show, Eq)

type Path = [Point]

solve15 contents = r
  where
    m = parse15 contents
    paths = trace ("map: " ++ show m) findPaths [Point 0 0] [] m
    costs = map (pathCost m . drop 1) paths
    r = length costs --minimum paths

pathCost (Map w h ll) [] = 0
pathCost m@(Map w h ll) (Point x y : rest) = ord ((ll !! y) !! x) + pathCost m rest

adjacents = [(-1, 0), (1, 0), (0, -1), (0, 1)]

findPaths :: [Point] -> [Point] -> Map -> [Path]
findPaths p@(pt : rest) done m | pt `elem` done = [] --  trace ("dead end: " ++ show p) []
findPaths path@(pt@(Point x y) : rest) done (Map w h _) | x == w - 1 && y == h - 1 = trace ("found: " ++ show path) [path]
findPaths path@(pt@(Point x y) : rest) done m@(Map w h _) = concat [findPaths (Point (x + dx) (y + dy) : path) (pt : done) m | (dx, dy) <- adjacents, x + dx >= 0 && x + dy < w && y + dy >= 0 && y + dy < h]
findPaths [] _ _ = error "empty path"

parse15 contents = Map w h ll
  where
    ll = lines contents
    w = length $ head ll
    h = length ll
