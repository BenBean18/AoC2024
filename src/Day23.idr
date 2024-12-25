module Day23

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Data.Nat
import Utilities
import Data.SortedMap
import Data.SortedSet
import Data.List1
import Data.Vect

-- Part 1

-- My computer is about to die, so I'm coding this on the M1 with no LSP installed (since installing pack over cellular data was taking forever and brew finished quicker)
-- as king george would say, "good luck!"

-- this is an undirected graph
-- finding triangles in the graph

-- filtering for computers that start with t
-- Only 286 possible computers (searched with regex "t[^-]")
-- ohh right adjacency matrix powers are paths of length n
-- so we could just sum up all entries along the diagonal where the vertex starts with t
-- i think this is more inefficient than just storing as adjacency list and checking for connections between vertices connected to a t[^-]

-- could try edge list of sorted pairs 

0 Computer : Type
Computer = Vect 2 Char

-- when parsing, make sure to add edge from a->b and from b->a
0 Graph : Type
Graph = SortedMap Computer (List Computer)

-- sorted map makes it easier to filter for t's? maybe? they'll all be in a row

-- argument: 
pairs : Ord a => List a -> List (a,a)
pairs (x :: xs) = map (\i => (x,i)) xs ++ pairs xs
pairs _ = []

edgeExists : Graph -> Computer -> Computer -> Bool
edgeExists m a b = (a `elem` (fromMaybe [] (lookup b m))) || (b `elem` (fromMaybe [] (lookup a m))) -- shouldn't need both

listify : (a,a) -> List a
listify (a,b) = [a,b]

edgesBetween : List Computer -> Graph -> List (List Computer)
edgesBetween l m = 
    let out = map listify (filter (uncurry (edgeExists m)) (pairs l)) in {-(trace $ show out)-} out

triangles : List Computer -> Graph -> SortedSet (List Computer)
triangles [] _ = empty
triangles (x::xs) m =
    let nextSet : List (List Computer) = map (x::) (edgesBetween (fromMaybe [] (lookup x m)) m)
        q : List (List Computer) = map sort nextSet in (triangles xs m) `union` (fromList q)

addEdge : Graph -> Vect 2 Computer -> Graph
addEdge g [a,b] = insertWith (++) b [a] (insertWith (++) a [b] g)

partial parseEdge' : List Char -> Vect 2 Computer
parseEdge' (a::b::'-'::c::d::xs) = [[a,b], [c,d]]

partial parseEdge : String -> Vect 2 Computer
parseEdge = parseEdge' . unpack

isT : Computer -> Bool 
isT ('t'::xs) = True
isT _ = False

partial part1 : String -> Int
part1 input = 
    let edges = map parseEdge (lines input)
        graph = foldl addEdge empty edges
        ts : List Computer = filter (isT) (keys graph)
        tris = triangles ts graph in cast (length (Data.SortedSet.toList tris))

-- Part 2

-- largest connected component!

-- i mean the naive way is to remove each consumed edge from the graph
-- it's just a BFS to consume each edge, could iterate through all edges but that's slower

removeEdge : Graph -> Vect 2 Computer -> Graph
removeEdge g [a,b] = 
    let newA = delete b $ fromMaybe [] (lookup a g)
        newB = delete a $ fromMaybe [] (lookup b g) in (insert a newA (insert b newB g))

removeVertex : Graph -> Computer -> Graph
removeVertex g v =
    let connectedTo = fromMaybe [] (lookup v g) in foldl (\graph, curVertex => insert curVertex (delete v $ fromMaybe [] (lookup curVertex g)) g) (delete v g) connectedTo

-- need visited set to avoid cycles
findConnected : Graph -> SortedSet Computer -> Computer -> List Computer
findConnected g visited current =
    if current `contains` visited then []
    else
        let neighbors = fromMaybe [] (lookup current g) in current :: concatMap (findConnected g (insert current visited)) neighbors

connectedComponents : Graph -> List (List Computer)
connectedComponents g = 
    case keys g of
        [] => []
        (x::xs) =>
            let next = findConnected g empty x in next :: connectedComponents (foldl removeVertex g next)

-- f*** i misread the problem
-- it's looking for the largest complete subgraph

partial part2 : String -> Int
part2 input = 
    let edges = map parseEdge (lines input)
        graph = foldl addEdge empty edges
        comps = sortBy (compare `on` length) $ connectedComponents graph in (trace $ show ((ne last) comps)) 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2