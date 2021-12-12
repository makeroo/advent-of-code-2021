module Main where

import Data.Char (isUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Debug.Trace (trace)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  print $ solveDay12 contents

solveDay12 contents = length $ allPaths ["start"] [] (parseGraph contents)

allPaths :: [String] -> [String] -> Map.Map String [String] -> [[String]]
-- allPaths p@("end":rest) _ _ = p
allPaths p@("end" : rest) done graph = [reverse p] -- trace ("done:" ++ show p)
allPaths p@(n : rest) done graph = res
  where
    reached = filter (`notElem` done) (graph Map.! n)
    newdone = if bigCave n then done else n : done
    res = concat [allPaths (x : p) newdone graph | x <- reached] -- trace ("path:" ++ show p ++ ", reached:" ++ show reached ++ ", newdone:" ++ show newdone)
allPaths [] _ _ = []

bigCave :: String -> Bool
bigCave [] = False
bigCave (a : rest) = isUpper a

parseGraph contents = res
  where
    edges_txt = map (parseEdge . splitOn "-") $ lines contents
    parseEdge [a, b] = (a, [b])
    parseEdge a = error $ "illegal input" ++ show a
    res_fwd = Map.toList $ Map.fromListWith (++) edges_txt
    res_bkw = [(f, [to]) | (to, x) <- res_fwd, f <- x]
    res = Map.fromListWith (++) $ res_fwd ++ res_bkw
