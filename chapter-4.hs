
-- https://emacs-lsp.github.io/lsp-mode/page/keybindings/

module Chapter4 where

import Debug.Trace

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
          where m = choose (a,b)


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

-- Ex 4.3

-- Section 4.2

bsearch1 f t = [(x,y) | x <- [0..t], y <- [0..t], t == f(x,y)]


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
        
