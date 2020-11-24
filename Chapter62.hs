
module Chapter62 where

import Data.List hiding (group)

-- https://stackoverflow.com/questions/30740366/list-with-random-numbers-in-haskell
import System.Random (randomRIO)

type Nat = Int

select :: Ord a => Nat -> [a] -> a
-- select k xs = (sort xs) !! (k - 1)

median xs = select k xs
  where
    k = (length xs + 1) `div` 2

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort xs = qsort us ++ vs ++ qsort ws
  where
    (us, vs, ws) = partition3 (pivot xs) xs


partition3 :: Ord a => a -> [a] -> ([a], [a], [a])
partition3 y = foldr op ([], [], [])
  where
    op x (us, vs, ws)
      | x < y = (x : us, vs, ws)
      | x == y = (us, x : vs, ws)
      | x > y = (us, vs, x : ws)


select k xs
  | k <= m = select k us
  | k <= m + n = vs !! (k - m - 1)
  | k > m + n = select (k - m - n) ws
  where
    (us, vs, ws) = partition3 (pivot xs) xs
    (m, n) = (length us, length vs)



group :: Nat -> [a] -> [[a]]
group n [] = []
group n xs = ys : group n zs
  where
    (ys, zs) = splitAt n xs

medians :: (Ord a) => [a] -> [a]
medians = map (middle . sort) . group 5
  where
    middle xs = xs !! ((length xs + 1) `div` 2 - 1)


pivot :: Ord a => [a] -> a
pivot [x] = x
pivot xs = median (medians xs)
  where
    median xs = select ((length xs + 1) `div` 2) xs



randomList :: Int -> IO ([Int])
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n - 1)
  return (r : rs)


main = do
  r <- randomList 1000
  return r
