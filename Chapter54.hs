module Chapter54 where

import qualified Chapter52 as M
import Data.Array

-- https://stackoverflow.com/questions/31299146/name-conflict-in-haskell-while-creating-modules
import Prelude hiding (Word)

type Word = [Char]

sortWords :: [Word] -> [Word]
sortWords = M.msort

-- >>> sortWords ["Alexandre","Roberto","Pedro","Jose","Jessica"]
-- ["Alexandre","Jessica","Jose","Pedro","Roberto"]


ordered :: Ord b => [a -> b] -> a -> a -> Bool
ordered [] x y = True
ordered (d:ds) x y = (d x < d y) || ((d x == d y) && ordered ds x y)

data Tree a = Leaf a | Node [Tree a] deriving Show

-- mktree :: (Bounded b, Enum b, Ord b) => [a -> b] -> [a] -> Tree [a]
-- mktree [] xs = Leaf xs
-- mktree (d:ds) xs = Node (map (mktree ds) (ptn d xs))

-- ptn :: (Bounded b, Enum b, Ord b) => (a -> b) -> [a] -> [[a]]
-- ptn d xs = [filter (\x -> d x == m) xs | m <- rng]
--   where
--     rng = [minBound .. maxBound]

-- ptn :: Ix b => (b,b) -> (a -> b) -> [a] -> [[a]]
-- ptn (l, u) d xs = elems xa
--   where
--     xa = accumArray snoc [] (l, u) (zip (map d xs) xs)
--     snoc xs x = xs ++ [x]

ptn (l, u) d xs = map reverse (elems xa)
  where
    xa = accumArray (flip (:)) [] (l, u) (zip (map d xs) xs)


flatten :: Tree [a] -> [a]
flatten (Leaf xs) = xs
flatten (Node ts) = concatMap flatten ts

-- bsort :: (Bounded b, Enum b, Ord b) => [a -> b] -> [a] -> [a]

-- bsort ds xs = flatten (mktree ds xs)
bsort bb [] xs = xs
-- bsort (d:ds) xs = concatMap (bsort ds) (ptn d xs)
bsort bb (d:ds) xs = concatMap (bsort bb ds) (ptn bb d xs)

-- rsort :: (Bounded b, Enum b, Ord b) => [a -> b] -> [a] -> [a]
-- rsort [] xs = xs
-- rsort (d:ds) xs = concat (ptn d (rsort ds xs))

rsort :: Ix b => (b, b) -> [a -> b] -> [a] -> [a]
rsort bb [] xs = xs
rsort bb (d:ds) xs = concat (ptn bb d (rsort bb ds xs))

