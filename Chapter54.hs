module Chapter54 where

import Chapter52 ( msort )

type LWord = [Char]

sortWords :: [LWord] -> [LWord]
sortWords = msort

-- >>> sortWords ["Alexandre","Roberto","Pedro","Jose","Jessica"]
-- ["Alexandre","Jessica","Jose","Pedro","Roberto"]

ordered :: Ord b => [a -> b] -> a -> a -> Bool
ordered [] x y = True
ordered (d:ds) x y = (d x < d y) || ((d x == d y) && ordered ds x y)

data Tree a = Leaf a | Node [Tree a]

mktree :: (Bounded b, Enum b, Ord b) => [a -> b] -> [a] -> Tree [a]
mktree [] xs = Leaf xs
mktree (d:ds) xs = Node (map (mktree ds) (ptn d xs))

ptn :: (Bounded b, Enum b, Ord b) => (a -> b) -> [a] -> [[a]]
ptn d xs = [filter (\x -> d x == m) xs | m <- rng]
 where rng = [minBound .. maxBound]

flatten :: Tree [a] -> [a]
flatten (Leaf xs) = xs
flatten (Node ts) = concatMap flatten ts

bsort :: (Bounded b, Enum b, Ord b) => [a -> b] -> [a] -> [a]
bsort ds xs = flatten (mktree ds xs)

