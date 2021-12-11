import System.IO
import Control.Monad
import Data.Char
import Data.List
import Debug.Trace

data OctoHeights = OctoHeights Int Int [Int] deriving (Show, Eq)


main = do
  contents <- readFile "day11.input"

  let octo_heights = parse contents

  print octo_heights
{-
  putStr $ (matrix octo_heights)

  let (t1, h1) = trig_flashes octo_heights

  print h1
  putStr $ (matrix t1)

  let (t2, h2) = trig_flashes t1

  print h2
  putStr $ (matrix t2)

  let                f = fst . trig_flashes
  let tx = iterate f octo_heights !! 5

  putStr $ (matrix tx)
-}
  let (_, r) = iterate f2 (octo_heights, 0) !! 100

  print r

  print (snd_star octo_heights)


snd_star (OctoHeights w h hh) = r
  where
    m = dropWhile (\(_, v, _) -> v < w * h) (iterate f3 ((OctoHeights w h hh), 0, 0))
    (_, _, r) = m !! 0

f3 (m, _, s) = (n, r, s+1)
  where
    (n, r) = trig_flashes m
  
f2 (m, v) = (n, v + r)
  where
    (n, r) = trig_flashes m


matrix (OctoHeights w h hh) = unlines x
  where
    x  = splitEvery w (map (\x -> chr(x+ord_zero)) hh)

splitEvery :: Int -> [a] -> [[a]]
splitEvery l a | length a < l = [a]
splitEvery l a = (take l a) : (splitEvery l (drop l a))

-- trace (show (flashed m1))
trig_flashes m = flashes (flashed m1) [] m1
  where
    m1 = incr m
    flashed (OctoHeights w _ hh) = ((map (\(x,y) -> (y,x))) . (map (\(idx,_) -> divMod idx w)) . (filter (\(idx, h) -> h > 7))) (zip [0..] hh)

{-
trace ("pt:"++show x++
                                                                ","++show y++
                                                                " - rest:"++show rest++
                                                                " - done:"++show done++
                                                                " - newflashes:"++show newflashes++
                                                                " - newhh:"++show newhh)
-}

flashes :: [(Int,Int)] -> [(Int,Int)] -> OctoHeights -> (OctoHeights, Int)
flashes [] _ (OctoHeights w h hh) = (OctoHeights w h [if v > 9 then 0 else v | v<- hh], length (filter (\h -> h > 9) hh))
flashes (pt:rest) done m | pt `elem` done = flashes rest done m
flashes ((x,y):rest) done (OctoHeights w h hh) | e > 9 =  flashes (rest++newflashes) ((x,y):done) (OctoHeights w h newhh)
  where
    m = OctoHeights w h hh
    e = energy_at m x y
    newflashes = neighbours_at m x y
    newhh = incr_idx newflashes hh
    incr_idx [] hh = hh
    incr_idx ((x,y):rest) hh = incr_idx rest (pre ++ [e] ++ post)
      where
        idx = y * w + x
        pre = take idx hh
        e = 1 + (hh !! idx)
        post = drop (idx + 1)  hh
flashes (pt:rest) done m = flashes rest done m


neighbours = [(x,y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

neighbours_at (OctoHeights w h _) x y = [(dx+x, dy+y) | (dx,dy) <- neighbours, dx+x >= 0 && dx+x < w && dy+y >= 0 && dy+y < h]

energy_at (OctoHeights w h hh) x y = hh !! (y * w + x)

incr (OctoHeights w h hh) = OctoHeights w h (map (1+) hh)

parse contents = OctoHeights (length (rows !! 0)) (length rows) heights
  where
    rows = lines contents
    heights = [ord(ch) - ord_zero | row <- rows, ch <- row]

ord_zero = ord('0')
