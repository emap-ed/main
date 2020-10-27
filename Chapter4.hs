
-- https://emacs-lsp.github.io/lsp-mode/page/keybindings/l

module Chapter4 where

import Debug.Trace ( trace ) 

type Nat = Int


search1, search2, search3, search4, search5 :: (Nat -> Nat) -> Nat -> [Nat]

search1 f t = [ x | x <- [0..t], t == f x]


search2 f t = seek (0,t)
  where seek (a,b) = [x | x <- [a..b], t == f x]

choose (a,b) = (a + b) `div` 2

search3 f t = seek (0,t)
  where seek (a,b)
          | a > b     = []
          | t < f m   = trace ("seek" ++ show (a,m-1)) seek (a, m - 1)
          | t == f m  = [m]
          | otherwise = trace ("seek" ++ show (m+1,b)) seek (m + 1, b)
          where
            m = choose (a,b)


bound :: (Nat -> Nat) -> Nat -> (Int, Nat)
bound f t = if t <= f 0 then (-1,0) else (b `div` 2, b)
  where b      = until done (* 2) 1
        done b = t <= f b


search4 f t = if f x == t then [x] else []
  where x = smallest (bound f t)
        smallest (a,b) = head [x | x <- [(a + 1)..b], t <= f x]


smallest (a,b) f t
  | a + 1 == b = b
  |   t <= f m = trace ("smallest " ++ show (a,m)) smallest (a,m) f t
  |  otherwise = trace ("smallest " ++ show (m,b)) smallest (m,b) f t
  where m = (a + b) `div` 2

search5 f t = if f x == t then [x] else []
  where x = smallest (bound f t) f t

-- Ex 4.2

smallest2 (a,b) f t = head ([x | x <- [a+1..m],t <= f x] ++
                            [x | x <- [m+1..b],t <= f x])
  where m = (a+b) `div` 2


-- Section 4.2

bsearch1 f t = [(x,y) | x <- [0..t], y <- [0..t], t == f (x,y)]

searchIn (a,b) f t = [(x,y) | x <- [a..t], y <- [b,b-1..0], t == f(x,y)]
bsearch2 f t = searchIn (0,t) f t


bsearch3 f t = searchIn (0,t)
  where
    searchIn (x,y)
      | x > t || y < 0 = []
      | z < t  = trace ("searchIn " ++ show (x+1,y)) searchIn (x+1,y)
      | z == t = (x,y) : trace ("searchIn " ++ show (x+1,y-1)) searchIn (x+1,y-1)
      | z > t  = trace ("searchIn " ++ show (x,y-1)) searchIn (x,y-1)
      where
        z = f(x,y)
        
bsearch4 f t = searchIn (0,t)
  where
    searchIn (x,y)
      | x > t || y < 0 = []
      | z < t  = searchIn (x+1,y)
      | z == t = (x,y) : searchIn (x+1,y-1)
      | z > t  = searchIn (x,y-1)
      where
        z = f(x,y)
        

bsearch5 f t = from (0,p) (q,0) where
  p = smallest (-1,t) (\y -> f (0,y)) t
  q = smallest (-1,t) (\x -> f (x,0)) t
  from (x1, y1) (x2, y2)
    | x2 < x1 || y1 < y2 = []
    | y1 - y2 <= x2 - x1 = row x
    | otherwise          = col y
    where
      x = smallest (x1 - 1,x2) (\x -> f (x,r)) t
      y = smallest (y2 - 1,y1) (\y -> f (c,y)) t
      c = (x1 + x2) `div` 2
      r = (y1 + y2) `div` 2
      row x
        | z < t  = from (x1,y1) (x2, r+1)
        | z == t = (x,r) : from (x1,y1) (x-1,r+1) ++ from (x+1,r-1) (x2,y2)
        | z > t  = from (x1,y1) (x-1,r+1) ++ from (x,r-1) (x2,y2)
        where z = f (x,r)
      col y
        | z < t  = from (c+1,y1) (x2, y2)
        | z == t = (c,y) : from (x1,y1) (c-1,y+1) ++ from (c+1,y-1) (x2,y2)
        | z > t  = from (x1,y1) (c-1,y) ++ from (c+1,y-1) (x2,y2)
        where z = f (c,y)


-- test bsearch5 (\(x,y) -> x^2 + 3^y) 20259
      

-- section 4.3

data Tree a = Null | Node (Tree a) a (Tree a)
 deriving Show

-- instance Show a => Show (Tree a) where
--   show Null    =  "N"
--   show (Node l x r) = "[" ++ show x ++ ", L:" ++ show l ++ ", R:" ++ show r ++ "]"

size :: Tree a -> Nat
size Null = 0
size (Node l x r) = 1 + size l + size r

flatten :: Tree a -> [a]
flatten Null = []
flatten (Node l x r) = (flatten l) ++ [x] ++ (flatten r)

search :: Ord k => (a -> k) -> k -> Tree a -> Maybe a
search key k Null = Nothing
search key k (Node l x r)
  | key x < k  = search key k r
  | key x == k = Just x
  | key x > k  = search key k l
  

height :: Tree a -> Nat
height Null = 0
height (Node l x r) = 1 + max (height l) (height r)


mktree :: Ord a => [a] -> Tree a
mktree [] = Null
mktree (x : xs) = Node (mktree ys) x (mktree zs)
  where (ys, zs) = partition (<x) xs

partition p xs = (filter p xs, filter (not . p) xs)
