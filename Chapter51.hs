
module Chapter51 where

data Tree a = Null | Node (Tree a) a (Tree a)

mktree :: Ord a => [a] -> Tree a
mktree []     = Null
mktree (x:xs) = Node (mktree ys) x (mktree zs)
  where
    (ys,zs) = partition (<x) xs

partition :: (a -> Bool) -> [a] -> ([a],[a])
partition p xs = foldr op ([],[]) xs
  where
    op x (ys,zs) = if p x then (x:ys,zs) else (ys,x:zs)


flatten :: Tree a -> [a]
flatten Null = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


qsort :: Ord a => [a] -> [a]
--qsort = flatten . mktree

qsort [] = []
qsort (x:xs) = qsort ys ++ [x] ++ qsort zs
  where
    (ys,zs) = partition (<x) xs
    
-- >>> qsort [12,4,6,7,8,90,0,1,7,8,2,8,9,3]
-- [0,1,2,3,4,6,7,7,8,8,8,9,12,90]

