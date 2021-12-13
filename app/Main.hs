module Main where

import Data.Char (isUpper)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
--import Debug.Trace (trace)
import System.Environment (getArgs)
import Data.List

trace x y = y

main :: IO ()
main = do
  args <- getArgs
  contents <- readFile $ head args

  print $ solveDay12 contents

  let x = solveDay12snd contents

--  putStr $ map $ show x
  --print $ x -- solveDay12snd contents
  print $ length x
--  print $ length $ nub x

solveDay12snd contents =   allPaths2 ["start"] [] [] (parseGraph contents)

allPaths2 :: [String] -> [String] -> [String] -> Map.Map String [String] -> [[String]]
allPaths2 p@("end":rest) done done2 graph = trace ("done2:" ++ show p) [reverse p]
allPaths2 p@(n: rest) done [] graph = res
  where
    reached = filter (\x -> x /= "start") (graph Map.! n)
    snd = filter (not . bigCave) $ intersect reached done
    fst = filter (\x -> x `notElem` done && x `notElem` snd) reached
    newdone = if bigCave n then done else n : done
    res = trace ("path2:" ++ show p ++ ", reached:" ++ show reached ++ ", fst:" ++ show fst ++ ", snd:"++show snd ++", newdone:"++show newdone ) concat ([allPaths2 (x : p) newdone [] graph | x <- fst] ++ [allPaths2 (x : p) newdone [x] graph | x <- snd])--
allPaths2 p done x graph = trace ("jump-"++show x) allPaths p done graph
    

solveDay12 contents = length $ allPaths ["start"] [] (parseGraph contents)

allPaths :: [String] -> [String] -> Map.Map String [String] -> [[String]]
-- allPaths p@("end":rest) _ _ = p
allPaths p@("end" : rest) done graph =trace ("done:" ++ show p) [reverse p] -- 
allPaths p@(n : rest) done graph = res
  where
    reached = filter (`notElem` done) (graph Map.! n)
    newdone = if bigCave n then done else n : done
    res = trace ("path:" ++ show p ++ ", reached:" ++ show reached ++ ", newdone:" ++ show newdone) concat [allPaths (x : p) newdone graph | x <- reached] -- 
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
