
module Chapter92 where

import Data.List (sortOn)

type Vertex = Int
type Weight = Int
type Graph = ([Vertex], [Edge])
type Edge = (Vertex, Vertex, Weight)

nodes :: Graph -> [Vertex]
nodes (vs, es) = vs

edges :: Graph -> [Edge]
edges (vs, es) = es

source, target :: Edge -> Vertex
source (u, v, w) = u
target (u, v, w) = v

weight :: Edge -> Weight
weight (u, v, w) = w

{-
alternative definitions

type Graph1 = Vertex → [(Vertex,Weight)]

taking Vertex as Integer

type Graph2 = Array Vertex [(Vertex,Weight)]

-- Ex convert between representations
-}

type Tree = Graph
type Forest = [Tree]


type State = (Forest, [Edge])

spats :: Graph -> [Tree]
spats =
  map extract . until (all done) (concatMap steps) . wrap . start


extract :: State -> Tree
extract ([t], _) = t

done :: State -> Bool
done = single . fst

start :: Graph -> State
-- start g = ([([v], []) | v <- nodes g], edges g)

wrap :: a -> [a]
wrap x = [x]

single :: [a] -> Bool
single [x] = True
single _   = False


steps :: State -> [State]
steps (ts, es) = [(add e ts, es') | (e, es') <- picks es, safeEdge e ts]

picks [] = []
picks (x:xs) = (x, xs) : [(y, x : ys) | (y, ys) <- picks xs]

safeEdge :: Edge -> Forest -> Bool
safeEdge e ts = find ts (source e) /= find ts (target e)

find :: Forest -> Vertex -> Tree
find ts v = head [t | t <- ts, any (== v) (nodes t)]

add :: Edge -> Forest -> Forest
add e ts = (nodes t1 ++ nodes t2, e : edges t1 ++ edges t2) : rest
  where
    t1 = find ts (source e)
    t2 = find ts (target e)
    rest = [t | t <- ts, t /= t1 && t /= t2]



gstep :: State -> State
gstep (ts, e:es) = if t1 /= t2 then (ts', es) else gstep (ts, es)
  where
    t1 = find ts (source e)
    t2 = find ts (target e)
    ts' = (nodes t1 ++ nodes t2, e : edges t1 ++ edges t2) : rest
    rest = [t | t <- ts, t /= t1 && t /= t2]


kruskal :: Graph -> Tree
kruskal = extract . until done gstep . start

start g = ([([v], []) | v <- nodes g], sortOn weight (edges g))


-- See https://en.wikipedia.org/wiki/Kruskal%27s_algorithm

