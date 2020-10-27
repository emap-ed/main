module Chapter53 where

data Tree a = Null | Node a (Tree a) (Tree a)

merge:: Ord a => [a] -> [a] -> [a]
merge [] ys  = ys
merge xs []  = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

flatten::Ord a => Tree a -> [a]
flatten Null = [ ]
flatten (Node x u v) = x : merge (flatten u) (flatten v)

heapify::Ord a => Tree a -> Tree a
heapify Null         = Null
heapify (Node x u v) = siftDown x (heapify u) (heapify v)

siftDown :: Ord a => a -> Tree a -> Tree a -> Tree a 
siftDown x Null Null = Node x Null Null

siftDown x (Node y u v) Null
 |x <= y = Node x (Node y u v) Null
 | otherwise = Node y (siftDown x u v) Null

siftDown x Null (Node y u v)
 |x <= y =Node x Null (Node y u v)
 | otherwise = Node y Null (siftDown x u v)

siftDown x (Node y ul ur) (Node z vl vr)
 | x <= min y z = Node x (Node y ul ur) (Node z vl vr) 
 | y <= min x z = Node y (siftDown x ul ur) (Node z vl vr) 
 | z <= min x y = Node z (Node y ul ur) (siftDown x vl vr)

hsort :: Ord a => [a] -> [a]
hsort = flatten . heapify . mktree

