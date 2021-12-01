import System.IO
import Control.Monad

-- https://stackoverflow.com/a/7867786

main = do
        contents <- readFile "day1.input"
        let depths = toInt ( words contents )
            increments = find_increments depths
        -- print depths
        print increments

toInt :: [String] -> [Int]
toInt = map read

find_increments :: [Int] -> Int
find_increments (a:b:c) | a>=b = find_increments (b:c)
find_increments (a:b:c) | a<b  = 1 + find_increments (b:c)
find_increments [a,b] | a<b  = 1
find_increments _            = 0
