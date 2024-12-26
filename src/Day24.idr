module Day24

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

-- this is stuff propagating through a control graph
-- topological sort
-- sometimes discussing random CS things becomes extremely helpful
-- "Gates wait until both inputs are received before producing output; wires can carry 0, 1 or no value at all."

-- Build a graph of all gates, then evaluate it in topological order
data Gate : Type where
    --  in1       in2       out       function
    G : String -> String -> String -> (Bool -> Bool -> Bool) -> Gate

Show Gate where
    show (G in1 in2 out fn) = in1 ++ " " ++ in2 ++ " -> " ++ out

output : Gate -> String
output (G _ _ out _) = out

-- the set of all nodes with no incoming edge is the ones which have values given

-- we can topologically sort and then evaluate everything
0 Graph : Type
Graph = SortedMap String (List String)

-- in1 -> out, in2 -> out
addGateToGraph : Graph -> Gate -> Graph
addGateToGraph graph (G in1 in2 out op) =
    insertWith (++) in1 [out] (insertWith (++) in2 [out] graph)

{-
Kahn's algorithm (Wikipedia):

L ← Empty list that will contain the sorted elements
S ← Set of all nodes with no incoming edge

while S is not empty do
    remove a node n from S
    add n to L
    for each node m with an edge e from n to m do
        remove edge e from the graph
        if m has no other incoming edges then
            insert m into S

if graph has edges then
    return error   (graph has at least one cycle)
else 
    return L   (a topologically sorted order)
 -}

-- edge list is O(n), adjacency list is O(n) to work backwards if we just check to see if the element in values
-- (searching every k-v pair would be O(n^2) I think, for every value for every key)
-- wait it's still n^2 since getting values is O(n)
-- oh well maybe it's fast enough, if not edge list (O(n) for neighbor lookup and working backwards) should work
topologicallySort : Graph -> List String -> List String
topologicallySort graph [] = []
topologicallySort graph (x::xs) =
    let neighbors : List String = fromMaybe [] (lookup x graph)
        newGraph : Graph = insert x [] graph -- no outgoing edges anymore
        v : List String = concat $ values newGraph
        goodNeighs = filter (\n => not (n `elem` v)) neighbors in
        x :: topologicallySort graph (goodNeighs ++ xs)

{-
x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
 -}

partial stringToOp : String -> (Bool -> Bool -> Bool)
stringToOp "AND" a b = a && b
stringToOp "XOR" a b = ((not a) && b) || (a && (not b))
stringToOp "OR" a b = a || b

partial parseGate : String -> Gate
parseGate s =
    let (in1::op::in2::_::out::[]) = words s in G in1 in2 out (stringToOp op)

0 State : Type
State = SortedMap String Bool

partial eval : State -> Gate -> State
eval s (G in1 in2 out fn) = 
    let (Just in1Val) = lookup in1 s -- partial, because if topo sort is bad this errors which is good
        (Just in2Val) = lookup in2 s
        in insert out (fn in1Val in2Val) s

partial parseStart : State -> String -> State
parseStart state s =
    let (k'::v'::[]) = words s
        k = (ne init) (unpack k') -- ignore colon
        v = v' == "1" in insert (pack k) v state

partial part1 : String -> Int
part1 input = 
    let (start ::: gateStrings) = splitOn "" (lines input)
        gates = map parseGate ((ne head) gateStrings)
        graph = foldl addGateToGraph empty gates
        s : List String = topologicallySort graph (keys graph)
        initial : State = foldl parseStart empty start
        final = foldl eval initial s in (trace $ show gates ++ "\n" ++ show graph ++ "\n" ++ show initial ++ "\n" ++ show s ++ "\n\n" ++ show final) 1

-- Part 2

partial part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2