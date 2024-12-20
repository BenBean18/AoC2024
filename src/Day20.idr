module Day20

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
import MinHeap

-- Part 1

-- how many paths of length 100 have start and end separated by one wall (i.e. adding an edge would turn it into a path of length 2)
-- find all pairs of coordinates separated by a wall and see if shortest path dist >= 100

-- potential coordinate pairs in real input (141x141): 197617140
-- potential dot pairs is 44845185

-- but we can just check every dot and a + around it
{-
  |
  |
--*--
  |
  |
 -}
-- then run dijkstra for all pairs

neighbors1 : (Int, Int) -> List (Int, Int)
neighbors1 j = map (Utilities.(+) j) [(0,1),(0,-1),(1,0),(-1,0)]

neighbors2 : (Int, Int) -> List (Int, Int)
neighbors2 j = map (Utilities.(+) j) [(0,2),(0,-2),(2,0),(-2,0)]

neighbors : SortedMap (Int, Int) Char -> (Int,Int) -> List (Int,Int)
neighbors m pos = filter (\p => '#' /= (fromMaybe '#' (lookup p m))) (neighbors1 pos)

-- more day 16 plagiarism! dijkstra is probably overkill but whatever
decreaseKey : Eq a => Ord a => Show a => BinaryHeap (Int,a) -> (Int,a) -> BinaryHeap (Int,a)
decreaseKey heap (prio,val) = 
    case findIndex (\(_,v) => v == val) heap of
        (Just idx') =>
            let idx : Fin (length heap) = idx'
                currentPrio : Int = fst (index' heap idx)
                h : BinaryHeap (Int,a) = (replaceWhen (\(_,v) => v == val) (min currentPrio prio, val) heap)
                upped = (heapifyUp h (finToNat idx) {p2=believe_me (InBounds (finToNat idx) h)}) in 
                upped
        _ => heap

-- should use memoization to get distances we've already computed, redoing the same path numerous times
-- ideally use a min-heap but this could be fast enough maybe
-- but...having 4 nodes for every point seems extremely inefficient...
partial dijkstra : SortedMap (Int, Int) Char -> BinaryHeap (Int,(Int,Int)) -> (Int, Int) -> Int
dijkstra _ [] _ = 0
dijkstra m unvisited end =
    let (Just (distance,next)) = findMin unvisited in --(trace $ show (next,distance)) $
            if next == end then distance
            else
                let neighs : List (Int, (Int, Int)) = map (distance+1,) (neighbors m next)
                    newUnvisited = foldl decreaseKey (deleteMin unvisited) neighs

                in 
                --(trace $ show (map fst newUnvisited) ++ "\n\n\n") $ 
                -- 0
                dijkstra m newUnvisited end

partial bfs : SortedMap (Int, Int) Char -> SortedSet (Int, Int) -> (Int, Int) -> (Int, Int) -> Int -> Int
bfs m visited current end cost = 
    if current == end then cost
    else if current `contains` visited then 0 else
    sum (map (\n => bfs m (insert current visited) n end (cost + 1)) (neighbors m current))

separatedByOneWall : SortedMap (Int, Int) Char -> (Int, Int) -> List ((Int, Int),(Int, Int))
separatedByOneWall m pos = if (fromMaybe '#' (lookup pos m)) == '#' then [] else
    let neighborPairs : List ((Int,Int), (Int,Int)) = zip (neighbors1 pos) (neighbors2 pos) -- (pos1,pos2)
        lookedUp : List (Char,Char) = zip (map (\k => fromMaybe '?' (lookup k m)) (neighbors1 pos)) (map (\k => fromMaybe '?' (lookup k m)) (neighbors2 pos)) -- (val1,val2)
        paired' : List (((Int,Int), (Int,Int)),(Char,Char)) = filter (\(_,chars) => chars == ('#','.') || chars == ('#','S') || chars == ('#','E')) (zip neighborPairs lookedUp)
        wallThenNot : List ((Int,Int), (Int,Int)) = map fst paired'
        pairs : List ((Int,Int), (Int,Int)) = map (pos,) (map snd wallThenNot) in pairs

partial findCheats : SortedMap (Int, Int) Char -> BinaryHeap (Int,(Int,Int)) -> List (Int, ((Int, Int), (Int, Int)))
findCheats m visitable =
    let l = keys m
        possibleCheats = concatMap (separatedByOneWall m) l
        -- -2 since we still have to go through wall
        timeSaved = map (\(a,b) => (trace $ show (a,b)) ((-2) + 
            --dijkstra m ((0,a)::visitable) b, (a,b)
            bfs m empty a b 0, (a,b)
            )) possibleCheats in --(trace $ show possibleCheats) 
        timeSaved

partial part1 : String -> Int
part1 input =
    let m = twoDStringToMap input
        l : List ((Int, Int), Char) = toList m
        visitable'' : List ((Int, Int), Char) = filter (\(k,v) => v /= '#') l
        visitable' : List (Int,Int) = (map fst visitable'')
        visitable = map (1000000000000000,) visitable'
        cheats = findCheats m visitable
        good = filter (>=the Int 100) (map fst cheats) in (trace $ show (sort (map fst cheats))) $ ((cast (length good)) `div` 2) -- double counting, so just divide by 2, perfect lol
        -- had to add start and end to double count correctly

-- 3012 is wrong
-- 1506 is also wrong (thought I was counting by x2)

-- 1507 is right, runtime: "real	2m38.923s" uhhhhhhh

-- Part 2

part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2