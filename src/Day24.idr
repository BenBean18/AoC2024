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
        x :: topologicallySort newGraph (goodNeighs ++ xs) -- recursing on updated value is always important lol

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

partial eval : State -> Maybe Gate -> State
eval s Nothing = s
eval s (Just gate@(G in1 in2 out fn)) =
    let (Just in1Val) = lookup in1 s -- partial, because if topo sort is bad this errors which is good
        (Just in2Val) = lookup in2 s
        in insert out (fn in1Val in2Val) s

partial parseStart : State -> String -> State
parseStart state s =
    let (k'::v'::[]) = words s
        k = (ne init) (unpack k') -- ignore colon
        v = v' == "1" in insert (pack k) v state

-- LSB first
binToInt : List Int -> Int
binToInt [] = 0
binToInt (x :: xs) = x + 2 * binToInt xs

-- also LSB first
showBin' : Int -> List Char
showBin' 0 = []
showBin' a = (if (a `mod` 2) == 0 then '0' else '1') :: showBin' (a `div` 2)

showBin : Int -> String
showBin = pack . showBin'

partial part1 : String -> Int
part1 input = 
    let (start ::: gateStrings) = splitOn "" (lines input)
        gates = map parseGate ((ne head) gateStrings)
        graph = foldl addGateToGraph empty gates
        initial : State = foldl parseStart empty start
        s : List String = topologicallySort graph (keys initial)
        gateMap : SortedMap String Gate = foldl (\cur, gate => insert (output gate) gate cur) empty gates
        gates : List (Maybe Gate) = map ((flip lookup) gateMap) s
        final : State = foldl eval initial gates
        zs = filter (\s => ((ne head) (unpack s)) == 'z') (keys final) -- should be correctly sorted
        bits = map (\z => if ((flip lookup) final z) == Just True then the Int 1 else 0) zs
        in --(trace $ show gates ++ "\n" ++ show graph ++ "\n" ++ show initial ++ "\n" ++ show s ++ "\n\n" ++ show final) 
        binToInt bits

-- Part 2

-- This is likely a ripple carry adder?
-- can't assume that though
-- probably try backtracking through and seeing where it goes wrong?

-- (x+y) ^ z gives bits that are different

-- Benchmark time on M1: 55737us for everything, including parsing

-- 220 gates, 24090 possible pairs, (24090 choose 4) possible sets of pairs to swap... = 1.4029035e+16
-- brute force is infeasible, which I suspected, but good to check I guess

-- can we split this into subgraphs, or is this one connected component?

toDot : Graph -> List String -> String
toDot _ [] = ""
toDot g (x::xs) = 
    let edges = concatMap (\e => x ++ " -> " ++ e ++ ";\n") (fromMaybe [] (lookup x g)) in edges ++ toDot g xs

checkOp : (Bool -> Bool -> Bool) -> String
checkOp f =
    if f False False then "??"
    else if (f False True) && not (f True True) then "XOR"
    else if not (f False True) then "AND"
    else "OR"

toDot' : List Gate -> String
toDot' [] = ""
toDot' ((G in1 in2 out op)::xs) = 
    let s = checkOp op in (in1 ++ " -> " ++ out ++ "_" ++ s ++ " -> "++out++";\n" ++ in2 ++ " -> " ++ out ++ "_" ++ s ++ " -> "++out++";\n") ++ toDot' xs


partial part2 : String -> Int
part2 input = 
    let (start ::: gateStrings) = splitOn "" (lines input)
        gates = map parseGate ((ne head) gateStrings)
        graph = foldl addGateToGraph empty gates
        initial : State = foldl parseStart empty start
        s : List String = topologicallySort graph (keys initial)
        gateMap : SortedMap String Gate = foldl (\cur, gate => insert (output gate) gate cur) empty gates
        gates : List (Maybe Gate) = map ((flip lookup) gateMap) s
        final : State = foldl eval initial gates
        xs = filter (\s => ((ne head) (unpack s)) == 'x') (keys final) -- should be correctly sorted
        xbits = reverse $ map (\z => if ((flip lookup) final z) == Just True then the Int 1 else 0) xs
        ys = filter (\s => ((ne head) (unpack s)) == 'y') (keys final) -- should be correctly sorted
        ybits = reverse $ map (\z => if ((flip lookup) final z) == Just True then the Int 1 else 0) ys
        zs = filter (\s => ((ne head) (unpack s)) == 'z') (keys final) -- should be correctly sorted
        bits = reverse $ map (\z => if ((flip lookup) final z) == Just True then the Int 1 else 0) zs
        in --(trace $ show gates ++ "\n" ++ show graph ++ "\n" ++ show initial ++ "\n" ++ show s ++ "\n\n" ++ show final) 
        --(trace $ (joinBy "" (map show xbits)) ++ "+" ++ (joinBy "" (map show ybits)) ++ "/=" ++ (joinBy "" (map show bits)) ++ "=" ++ show (binToInt (reverse bits))) 2
        -- (trace $ (toDot graph (keys graph))) 2
        (trace $ (toDot' (map (fromMaybe (G "" "" "" (\a, b => a && b))) gates))) 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2