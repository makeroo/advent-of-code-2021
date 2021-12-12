data Tree a
  = Empty
  | Node a [Tree a]

preorderTree :: Tree a -> [a]
preorderTree Empty = []
preorderTree (Node a children) = a : concatMap preorderTree children

main = do
  print $ preorderTree (Node "a" [Node "aa" [Node "aaa" [], Node "aab" []], Node "ab" []])