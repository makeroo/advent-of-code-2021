import Data.List

reduce :: (a -> a -> a) -> a -> [a] -> a
reduce _ v [] = v
reduce f v (a:rest) = reduce f (f v a) rest

reduce2 :: (a -> a -> a) -> [a] -> Maybe a
reduce2 _ [] = Nothing
reduce2 _ [a] = Just a
reduce2 f (a:b:rest) = reduce2 f ((f a b):rest)

unionx :: [String] -> String
unionx [] = ""
unionx [a] = a
unionx (a:b:rest) = unionx ((union a b):rest)

main = do

  print (reduce (\a b -> a + b) 1 [1,2,3])
  print (reduce2 (\a b -> a + b) [1,1,2,3])
  print (unionx ["ciao", "bello"])
