
module Chapter61 where

import Chapter52 (halve, single, unwrap)

minimum, maximum :: Ord a => [a] -> a
-- minimum = foldr1 min
-- maximum = foldr1 max

minimum (x:xs) = foldr min x xs
maximum (x:xs) = foldr max x xs

minmax :: Ord a => [a] -> (a, a)
-- minmax (x:xs) = foldr op (x, x) xs
--   where
--     op x (y, z) = (min x y, max x z)

-- minmax (x:xs) = foldr op (x, x) xs
--   where
--     op x (y, z)
--       | x < y = (x, z)
--       | z < x = (y, x)
--       | otherwise = (y, z)
      
-- best and worst cases?

-- minmax [x] = (x, x)
-- minmax [x, y] =
--   if x <= y
--     then (x, y)
--     else (y, x)
-- minmax xs = (min a1 a2, max b1 b2)
--   where
--     (a1, b1) = minmax ys
--     (a2, b2) = minmax zs
--     (ys, zs) = halve xs


minmax = unwrap . until single (pairWith op) . mkPairs
  where
    op (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)

pairWith f [] = []
pairWith f [x] = [x]
pairWith f (x:y:xs) = f x y : pairWith f xs

mkPairs [] = []
mkPairs [x] = [(x,x)]
mkPairs (x:y:xs) =
  if x <= y
    then (x, y) : mkPairs xs
    else (y, x) : mkPairs xs
