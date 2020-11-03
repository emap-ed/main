module Chapter53 where

data Tree a = Null | Node a (Tree a) (Tree a) deriving Show

type Nat = Int

merge:: Ord a => [a] -> [a] -> [a]
merge [] ys  = ys
merge xs []  = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y : ys)
  | otherwise = y : merge (x : xs) ys

flatten :: Ord a => Tree a -> [a]
flatten Null = []
flatten (Node x u v) = x : merge (flatten u) (flatten v)

heapify :: Ord a => Tree a -> Tree a
heapify Null         = Null
heapify (Node x u v) = siftDown x (heapify u) (heapify v)

siftDown :: Ord a => a -> Tree a -> Tree a -> Tree a 
siftDown x Null Null = Node x Null Null

siftDown x (Node y u v) Null
  | x <= y = Node x (Node y u v) Null
  | otherwise = Node y (siftDown x u v) Null

siftDown x Null (Node y u v)
  | x <= y = Node x Null (Node y u v)
  | otherwise = Node y Null (siftDown x u v)

siftDown x (Node y ul ur) (Node z vl vr)
  | x <= min y z = Node x (Node y ul ur) (Node z vl vr)
  | y <= min x z = Node y (siftDown x ul ur) (Node z vl vr)
  | z <= min x y = Node z (Node y ul ur) (siftDown x vl vr)


-- mktree [] = Null
-- mktree (x:xs) = Node x (mktree (take m xs)) (mktree (drop m xs))
--   where
--     m = length xs `div` 2


-- mkpair n xs = (mktree (take n xs), drop n xs)
mkpair :: Nat -> [a] -> (Tree a, [a])
mkpair 0 xs = (Null, xs)
mkpair n (x:xs) = (Node x l r, zs)
  where (l, ys) = mkpair m xs
        (r, zs) = mkpair (n - 1 - m) ys
        m = (n - 1) `div` 2

mktree xs = fst (mkpair (length xs) xs)    


hsort :: Ord a => [a] -> [a]
hsort = flatten . heapify . mktree

