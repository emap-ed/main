
module Chapter51 where

import Debug.Trace ( trace ) 


data Tree a = Null | Node (Tree a) a (Tree a) deriving Show

mktree :: Ord a => [a] -> Tree a
mktree []     = Null
mktree (x:xs) = Node (mktree ys) x (mktree zs)
  where
    (ys,zs) = partition (<x) xs

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs = foldr op ([],[]) xs
  where
    op x (ys,zs) = if p x then (x:ys,zs) else (ys,x:zs)

-- >>> mktree [3,4,2,1]
-- Node (Node (Node Null 1 Null) 2 Null) 3 (Node Null 4 Null)

flatten :: Tree a -> [a]
flatten Null = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

qsort :: Ord a => [a] -> [a]
-- qsort = flatten . mktree
-- qsort xs = flatten (mktree xs)

qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
  where
    (ys,zs) = partition (<x) xs
    
-- >>> qsort [12,4,6,7,8,90,0,1,7,8,2,8,9,3]
-- [0,1,2,3,4,6,7,7,8,8,8,9,12,90]


{- Exercicio 5.2

qsort = flatten . mktree

qsort (x:xs) =
flatten . mktree (x:xs) = 
flatten (mktree x:xs) =

flatten (Node (mktree ys) x (mktree zs)
  where
    (ys,zs) = partition (<x) xs) =

flatten (mktree ys) ++ [x] ++ flatten (mktree zs)
  where
    (ys,zs) = partition (<x) xs) =

flatten . mktree ys ++ [x] ++ flatten . mktree zs
  where
    (ys,zs) = partition (<x) xs) =

qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
  where
    (ys,zs) = partition (<x) xs) =

-}