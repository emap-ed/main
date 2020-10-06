
module Chapter2 where

append :: [a] -> [a] -> [a]
append [] [] = []
append [] ys = ys
append xs [] = xs
append (x:xs) ys = x : (append xs ys)

concat1, concat2 :: [[a]] -> [a]
concat1 = foldr (++) []
concat2 = foldl (++) []

concat1r [] = [] 
concat1r (xs:xss) = xs ++ concat1r xss

concat2r xss = step [] xss 
step ws [] = ws
step ws (xs:xss) = step (ws ++ xs) xss

inits,tails :: [a] -> [[a]]

inits[] = [[]] 
inits(x:xs)= [] : map (x:) (inits xs)

tails[] = [[]]
tails (x:xs) = (x:xs):tails xs

-- >>> inits "abcdee"
-- ["","a","ab","abc","abcd","abcde","abcdee"]

{-
concat2r [[1,2],[2,4]] =
step [] [[1,2],[2,4]] = 
step [] ++ [1,2] [[2,4]] = 
step (([] ++ [1,2]) ++ [2,4]) [] = (([] ++ [1,2]) ++ [2,4])
-}  

-- >>> concat1 [[1..10],[10,20],[2]]
-- [1,2,3,4,5,6,7,8,9,10,10,20,2]

-- >>> concat1a [[1..10],[10,20],[2]]
-- [1,2,3,4,5,6,7,8,9,10,10,20,2]
