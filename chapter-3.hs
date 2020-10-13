
-- https://emacs-lsp.github.io/lsp-mode/page/keybindings/

module Chapter3 where

type Nat = Int
type SymList a = ([a],[a])

cons :: a -> [a] -> [a]
cons x xs = x:xs

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]


single :: [a] -> Bool
single [x] = True
single _   = False

-- abstraction function
fromSL :: SymList a ->  [a]
fromSL (xs, ys) = xs ++ reverse ys


snocSL :: a -> SymList a ->  SymList a
snocSL x (xs,ys) = if null xs then (ys,[x]) else (xs,x:ys)

-- no error for empty list?
lastSL :: SymList a -> a
lastSL (xs, ys) = if null ys then head xs else head ys


-- second clause works because
-- [] ++ reverse (us ++ vs) = reverse vs ++ reverse us

tailSL :: SymList a -> SymList a
tailSL (xs,ys)
  |   null xs = if null ys then (error "empty list") else nilSL
  | single xs = (reverse vs, us)
  | otherwise = (tail xs, ys)
  where (us, vs) = splitAt (length ys `div` 2) ys

initSL :: SymList a -> SymList a
initSL (xs,ys)
  |   null ys = if null xs then (error "empty list") else nilSL
  | single ys = (us, reverse vs)
  | otherwise = (xs, tail ys)
  where (us, vs) = splitAt (length xs `div` 2) xs

nilSL :: SymList a
nilSL = ([],[])

nullSL :: SymList a -> Bool
nullSL (xs, ys) = null xs && null ys

singleSL :: SymList a -> Bool
singleSL (xs, ys) = (null xs && single ys) || (null ys && single xs)
  
lengthSL :: SymList a -> Nat
lengthSL (xs, ys) = length xs + length ys

-- exercicio 3.3
consSL :: a -> SymList a  -> SymList a
consSL x (xs,ys) = if null ys then ([x],xs) else (x:xs,ys)

headSL :: SymList a -> a
headSL (xs, ys) = if null xs then head ys else head xs

-- >>> consSL 1 nilSL
-- ([1],[])
-- >>> consSL 2 $ consSL 1 nilSL
-- ([2],[1])
-- >>> consSL 3 $ consSL 2 $ consSL 1 nilSL
-- ([3,2],[1])
-- >>> consSL 4 $ consSL 3 $ consSL 2 $ consSL 1 nilSL
-- ([4,3,2],[1])
-- >>> consSL 5 $ consSL 4 $ consSL 3 $ consSL 2 $ consSL 1 nilSL
-- ([5,4,3,2],[1])
-- >>> initSL $ consSL 5 $ consSL 4 $ consSL 3 $ consSL 2 $ consSL 1 nilSL
-- ([5,4],[2,3])

--- use case

-- quadractic for length
inits1 :: [a] -> [[a]]
inits1 [] = [[]]
inits1 (x:xs) = [] : map (x:) (inits1 xs)

-- >>> inits1 [1..5]
-- [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]

-- not online, we can't reverse infinite list
-- inits = map reverse . reverse . tails . reverse

-- linear for length
inits2 :: [a] -> [[a]]
inits2 = map fromSL . scanl (flip snocSL) nilSL

-- >>> inits2 [1..5]
-- [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]

-- linear for length without SL (ex 3.7)
inits3 :: [a] -> [[a]]
inits3 = map reverse . scanl (flip (:)) []

-- >>> inits3 [1..5]
-- [[],[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]



fetch :: Nat -> [a] -> a
fetch k xs = if k == 0 then head xs else fetch (k - 1) (tail xs)


data Tree a = Leaf a | Node Nat (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf x) = show x
  show (Node x t1 t2) = "[" ++ show x ++ "," ++ show t1 ++ ","  ++ show t2 ++ "]"

size :: Tree a -> Nat
size (Leaf x) = 1 
size (Node n _ _) = n

node :: Tree a -> Tree a -> Tree a
node t1 t2 = Node (size t1 + size t2) t1 t2

data Digit a = Zero | One (Tree a)
type RAList a = [Digit a]

instance Show a => Show (Digit a) where
  show Zero    =  "Zero"
  show (One t) = "One" ++ show t

fromRA :: RAList a -> [a]
fromRA = concatMap from
  where from Zero    = []
        from (One t) = fromT t

-- fromT :: Tree a -> [a]
-- fromT (Leaf x) = [x]
-- fromT (Node _ t1 t2) = fromT t1 ++ fromT t2

-- exercise 3.8
fromT :: Tree a -> [a]
fromT t = fromTs [t]
fromTs :: [Tree a] -> [a]
fromTs [] = []
fromTs (Leaf x : ts) = x : fromTs ts
fromTs (Node _ t1 t2 : ts) = fromTs (t1 : t2 : ts)
  

fetchRA :: Nat -> RAList a -> a
fetchRA k (Zero:xs) = fetchRA k xs
fetchRA k (One t:xs) = if k < size t
                       then fetchT k t
                       else fetchRA (k - size t) xs

fetchT :: Nat -> Tree a -> a
fetchT 0 (Leaf x)       = x
fetchT k (Node n t1 t2) = if k < n
                          then fetchT k t1
                          else fetchT (k - m) t2
  where
    m = n `div` 2

-- now we have
-- fetch k . fromRA = fetchRA k

-- exercicse 3.9

nullRA :: RAList a -> Bool
nullRA [Zero] = True
nullRA _ = False

nilRA :: RAList a
nilRA = [Zero]

-- Exercise 3.10
toRA :: [a] -> RAList a
toRA = foldr consRA nilRA

-- exercise 3.11
-- updateRA :: Nat -> a -> RAList a -> RAList a

consRA :: a -> RAList a -> RAList a
consRA x xs = consT (Leaf x) xs

consT :: Tree a -> RAList a -> RAList a
consT t1 []          = [One t1]
consT t1 (Zero:xs)   = One t1 : xs
consT t1 (One t2:xs) = Zero : consT (node t1 t2) xs


unconsRA :: RAList a -> (a, RAList a)
unconsRA xs = (x, ys)
  where (Leaf x, ys) = unconsT xs

unconsT :: RAList a -> (Tree a, RAList a)
unconsT (One t : xs) = if null xs then (t, []) else (t, Zero : xs)
unconsT (Zero : xs)  = (t1, One t2 : ys)
  where (Node _ t1 t2, ys) = unconsT xs


-- Exercise 3.13 Define headRA and tailRA.
-- headRA :: RAList a -> a
-- tailRA :: RAList a -> RAList a

