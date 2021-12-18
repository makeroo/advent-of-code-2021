import System.Environment (getArgs)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Debug.Trace (trace)

main = do
  args <- getArgs
  contents <- readFile $ head args

  let (fst_seq, rules) = parseInput14 contents

  print fst_seq
  print  rules
{-
  let r2 = apply_rules fst_seq rules

  print r2

  let r3 = apply_rules r2 rules

  print r3
  -}
{-
  let fseq = iterate x (fst_seq, rules) !! 10
      x (seq, rules) = (apply_rules seq rules, rules)
      freqs = Map.fromListWith (+) [(c, 1) | c <- fst fseq]
      max = maximum $ Map.elems freqs
      min = minimum $ Map.elems freqs

  print freqs
  print (max - min)
-}
{-  
  let fseq = iterate x (take 2 fst_seq, rules) !! 40
      x (seq, rules) = trace ("iter: " ++ show (length seq) ++ show (calc_freqs seq)) (apply_rules seq rules, rules)
      freqs = Map.fromListWith (+) [(c, 1) | c <- fst fseq]
      max = maximum $ Map.elems freqs
      min = minimum $ Map.elems freqs

  print freqs
  print (max - min)
-}

{-
[
(('B',3221791993050),('B',1610895996525)),(('C',3974261876818),('C',1987130938409)),(('F',7356733041787),('F',3678366520894)),(('H',4058165584532),('H',2029082792266)),(('K',5031117902580),('K',2515558951290)),(('N',1605402636912),('N',802701318456)),(('O',3958086971233),('O',1979043485616)),(('P',4049559693384),('P',2024779846692)),(('S',2765918838422),('S',1382959419211)),(('V',5760403316770),('V',2880201658385))]

-}
  fmap = fst (iterate step_freqs_req (seq, rules) !! 40)
  fchar = [[(a,v),(b,v)] | (a:b:[],v) <- Map.toList fmap]
  charfreq = Map.fromListWith (+) (concat fchar)
  submap = [b | (a,b) <- Map.toList fchar]
  submap2 = Map.fromListWith (+) submap
  a = zip (Map.toList charfreq) (Map.toList submap2)
  b = [ x-y | ((l,x),(l2,y)) <- a]
  max = maximum $ Map.elems charfreq
  min = minimum $ Map.elems charfreq
  print max - min -- too low

step_freqs :: Map.Map String Int -> Map.Map String String -> Map.Map String Int
step_freqs input rules = output
  where
    in_list = Map.toList input
    in_res = [(s, m, rules Map.! s) | (s, m) <- in_list]
    next_list = [(a:s:b:[], m) | (a:b:[], m, [s]) <- in_res]
    next_prep = [(Map.toList (calc_freqs s), m) | (s, m) <- next_list]
    next_o = concatMap mult_freq next_prep
    mult_freq ((x,y):rest, v) = (x, y * v):mult_freq (rest, v)
    mult_freq ([], v) = []
    output = Map.fromListWith (+) next_o

step_freqs_req (input, rules) = (step_freqs input rules, rules)

--measure_couple seq 

apply_rules_rec (seq, rules) = (apply_rules seq rules, rules)
{-
  let fseq = iterate x (fst_seq, rules) !! 40
      x (seq, rules) = trace ("iter: " ++ show (length seq) ++ show (calc_freqs seq)) (apply_rules seq rules, rules)
      freqs = Map.fromListWith (+) [(c, 1) | c <- fst fseq]
      max = maximum $ Map.elems freqs
      min = minimum $ Map.elems freqs
-}

calc_freqs seq = Map.fromListWith (+) [ (take 2 $ drop n seq, 1) | n <- [0..length seq - 2] ]

apply_rules :: String -> Map.Map String String -> String
apply_rules seq rules =  concat r -- trace ("coup:"++show couples++", ins:"++show inserts)
  where
    couples = [ take 2 $ drop n seq | n <- [0..length seq - 1] ]
    inserts = map (\e -> Map.lookup e rules) couples
    r = map apply_inserts $ zip couples inserts

apply_inserts :: (String, Maybe String) -> String
apply_inserts ((a:b:rest), Just x) = [a] ++ x
apply_inserts (a, _) = a
  
parseInput14 :: String -> (String, Map.Map String String)
parseInput14 contents = (seq, rules)
  where
    (seq:_:rules_txt) = lines contents
    rules = Map.fromList $ map (tuple2 . splitOn " -> ") rules_txt

tuple2 [a,b] = (a,b)
