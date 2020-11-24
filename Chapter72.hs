
module Chapter72 where

import Data.List ( tails )

{-
mcc :: [Component] -> Candidate
mcc = minWith cost . candidates


candidates :: [Component] -> [Candidate]
candidates xs = foldr step [c0] xs
  where
    step x cs = concatMap (extend x) cs


extend :: Component -> Candidate -> [Candidate]

{-
 A greedy algorithm for computing mcc arises as the result of
 successfully fus- ing minWith cost with candidates. Operationally
 speaking, instead of building the complete list of candidates and
 then selecting a best one, we construct a single best candidate at
 each step.
-}

mcc = foldr gstep c0
  where
    gstep x = minWith cost . extend x


The greedy condition:

 minWith cost (map (gstep x) cs) = gstep x (minWith cost cs)

-}


sort :: Ord a => [a] -> [a]
-- sort = minWith ic . perms

ic :: Ord a => [a] -> Int
ic xs = length [(x, y) | (x, y) <- pairs xs, x > y]

-- >>> ic [4,5,1,3]
-- 4

pairs :: [a] -> [(a, a)]
pairs xs = [(x, y) | x:ys <- tails xs, y:zs <- tails ys]

-- >>> pairs [1,2,3]
-- [(1,2),(1,3),(2,3)]

perms :: [a] -> [[a]]
perms = foldr (concatMap . extend) [[]]

extend :: a -> [a] -> [[a]]
extend x [] = [[x]]
extend x (y:xs) = (x : y : xs) : map (y :) (extend x xs)

-- >>> perms [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]

minWith :: Ord b => (a -> b) -> [a] -> a
minWith f = foldr1 (smaller f)
  where
    smaller f x y =
      if f x <= f y
        then x
        else y

-- gstep :: Ord a => a -> [a] -> [a]
-- gstep x = minWith ic . extend x

-- both lists below have 3 inversions
-- >>> map ic [[7,1,2,3],[3,2,1,7]]
-- [3,3]

-- >>> gstep 6 [7,1,2,3]
-- [7,1,2,3,6]

-- >>> gstep 6 [3,2,1,7] 
-- [3,2,1,6,7]

-- >>> minWith ic (map (gstep 6) [[7,1,2,3],[3,2,1,7]])
-- [3,2,1,6,7]

-- >>> gstep 6 (minWith ic [[7,1,2,3],[3,2,1,7]])
-- [7,1,2,3,6]


--- Insertion sort

-- sort = (minWith id) . perms
-- sort = minimum . perms

gstep x [] = [x]
gstep x (y:xs) =
  if x <= y
    then x : y : xs
    else y : gstep x xs

sort = foldr gstep []


{- Selection sort

perms [] = [[]]
perms xs = concatMap subperms (picks xs)
  where
    subperms (x, ys) = map (x :) (perms ys)

picks [] = []
picks (x:xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

sort [] = []
sort xs = x : sort ys
  where
    (x, ys) = pick xs
    pick xs = minimum (picks xs)

-}
