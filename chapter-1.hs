{-  motivacao  -}

-- fibonacci

fib1 :: Double -> Double
fib1 0 = 0
fib1 1 = 1
fib1 n = fib1 (n - 2) + fib1 (n - 1)

fib3 :: Double -> Double
fib3 n = aux 1 0 n
  where
    aux a b c
      | c == 0 = b
      | otherwise = aux (a + b) a (c - 1)
  

-- factorial

fact1 :: Double -> Double
fact1 0 = 1
fact1 n = n * fact1 (n - 1)

fact1' :: Double -> Double
fact1' n
  | n == 0 = 1
  | n > 0 = n * fact1' (n - 1)


fact2 :: Double -> Double
fact2 n = product [1..n]


{-

 tipos e funções

 :t para inspecionar tipos
 :r para recarregar arquivo

 Funções: map, filter e foldr/foldl

-}

type Nat = Int

soma n = n + 10

test0 = map soma [1..100]
test1 = map (\x -> x + 10) [1..100]
test2 = filter odd [1..100]

-- soma de 1..10
test3 = foldr (+) 0 [1..10]

label :: [a] -> [(Nat,a)] -- se tirar esta linha?
label xs = zip [0..] xs

test4 = label [1..10]

-- incomplete input for foldr!
length' :: [a] -> Nat 
length' = foldr succ 0 
  where succ x n = n + 1

  
{- list processing -}

concat1, concat2 :: [[a]] -> [a]
concat1 = foldr (++) []
concat2 = foldl (++) []

append [] ys = ys
append (x:xs) ys = x : (append xs ys)

-- qual é melhor? direcao as vezes é importante. Veja o custo de
-- append. Use `:set +s`


{- inductive and recursive definitions -}

perms1 [] = [[]]
perms1 (x:xs) = [zs | ys <- perms1 xs, zs <- inserts x ys]

inserts :: a -> [a] -> [[a]]
inserts x []     = [[x]]
inserts x (y:ys) = (x : y : ys) : map (y:) (inserts x ys)

perms2 :: [a] -> [[a]]
perms2 = foldr step [[]]
  where
    step x xs = concatMap (inserts x) xs

perms3 :: [a] -> [[a]]
perms3 = foldr (concatMap . inserts) [[]]


perms4 :: [a] -> [[a]]
perms4 [] = [[]]
perms4 xs = [ x : zs | (x,ys) <- picks xs, zs <- perms4 ys]

picks :: [a] -> [(a,[a])]
picks [] = []
picks (x:xs) = (x,xs) : [(y, x:ys) | (y,ys) <- picks xs]

perms5 :: [a] -> [[a]]
perms5 [] = [[]]
perms5 xs = concatMap subperms (picks xs)
  where
    subperms (x,ys) =  map (x:) (perms5 ys)


-- until (> 100) (+ 2) 1
while p = until (not . p)
-- while (< 100) (+ 2) 1


{- fusion -}


-- map f . map g = map (f . g)
-- concatMap f . map g = concatMap (f . g)
-- foldr f e . map g = foldr (f . g) e

z0 = map (\x -> x * 2) . map (\x -> x + 3)
z1 = map ((\x -> x * 2) . (\x -> x + 3))

-- foldr f e (xs ++ ys) = foldr f (foldr f e ys) xs
-- foldr f e . concat = foldr (flip (foldr f )) e


{- accumulating and tupling -}


collapse1 :: [[Int]] -> [Int]
collapse1 xss = help1 [] xss

help1 xs xss = if sum xs > 0 || null xss then xs
               else help1 (xs ++ head xss) (tail xss)


collapse2 :: [[Int]] -> [Int]
collapse2 xss = help2 (0,[]) $ labelsum xss

help2 (s,xs) xss = if s > 0 || null xss then xs
                   else help2 (cat (s,xs) (head xss)) (tail xss)

cat (s,xs) (t,ys) = (s + t, xs ++ ys)
labelsum xss = zip (map sum xss) xss


collapse3 :: [[Int]] -> [Int]
collapse3 xss = help2 (0,[]) $ labelsum xss

help3 (s,f) xss = if s > 0 || null xss then f
                  else help3 (s + t, f . (xs ++)) (tail xss)
  where
    (t,xs) = head xss



{-
- https://link.springer.com/chapter/10.1007/3-540-59451-5_7
- https://www.amazon.com/Algebra-Programming-Prentice-hall-International-Computer/dp/013507245X
-}

foldn :: (a,a -> a) -> (Integer -> a)
foldn (z, s) = func
  where
    func 0 = z
    func n = s $ func (n - 1)

plus m = foldn (m, succ)
mult m = foldn (0, plus m)
expn m = foldn (1, mult m)

fact3 :: Integer -> Integer
fact3 = snd . foldn ((0,1), f)
  where
    f (m,n) = (m + 1, (m + 1) * n)

fib2 :: Integer -> Integer
fib2 = fst . foldn ((1,1), f)
  where
    f (m,n) = (n, m + n)


-- lambdas

alambda = (\x -> (1, 2 + x)) 12

-- lists

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) 
  | x <= y = x : y : ys
  | otherwise = y : insert x ys


isort :: Ord a => [a] -> [a]
isort []  = []
isort (x:xs) = insert x (isort xs)



-- outros exemplos

div2 :: Int -> Maybe Int
div2 x 
  | mod x 2 == 0 = Just (div x 2)
  | otherwise    = Nothing


div4 :: Int -> Maybe Int
div4 x = div2 x >>= div2

div8 x = div2 x >>= div2 >>= div2

my :: [a] -> [a]
my (x:xs) = xs

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], mod n x == 0]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

hailstone :: Int  -> [Int]
hailstone 1 = [1]
hailstone n =
  if n `mod` 2 == 0 then
   n : hailstone (quot n 2)
  else
   n : hailstone (n * 3 + 1)

