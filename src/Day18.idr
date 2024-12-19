module Day18

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

-- unabashed Day 16 self-plagiarism
neighbors' : (Int, Int) -> List (Int, Int)
neighbors' j = map (Utilities.(+) j) [(0,1),(0,-1),(1,0),(-1,0)]

neighbors : SortedMap (Int, Int) Char -> (Int,Int) -> List (Int,(Int,Int))
neighbors m pos = map (1,) $ filter (\p => '#' /= (fromMaybe '.' (lookup p m))) $ filter (\(x,y) => x >= 0 && x <= 70 && y >= 0 && y <= 70) (neighbors' pos)

decreaseKey : BinaryHeap (Int,(Int,Int)) -> (Int,(Int,Int)) -> BinaryHeap (Int,(Int,Int))
decreaseKey heap (prio,val) = 
    case findIndex (\(_,v) => v == val) heap of
        (Just idx') =>
            let idx : Fin (length heap) = idx'
                currentPrio : Int = fst (index' heap idx)
                h : BinaryHeap (Int,(Int,Int)) = (replaceWhen (\(_,v) => v == val) (min currentPrio prio, val) heap)
                upped = (heapifyUp h (finToNat idx) {p2=believe_me (InBounds (finToNat idx) h)}) in 
                -- (trace $ show h ++ "\n\n" ++ show upped) 
                upped
        _ => heap

partial dijkstra : SortedMap (Int, Int) Char -> BinaryHeap (Int,(Int,Int)) -> (Int, Int) -> Int
dijkstra _ [] _ = 0
dijkstra m unvisited end =
    let (Just (distance,next)) = findMin unvisited in --(trace $ show (next,distance)) $
            if next == end then distance
            else
                let neighs : List (Int, (Int,Int)) = map (\(d,pos) => (d+distance,pos)) (neighbors m next)
                    newUnvisited = foldl decreaseKey (deleteMin unvisited) neighs

                in 
                --(trace $ show (map fst newUnvisited) ++ "\n\n\n") $ 
                -- 0
                dijkstra m newUnvisited end

-- yay folds
addObstacle : SortedMap (Int, Int) Char -> (Int, Int) -> SortedMap (Int, Int) Char
addObstacle m p = insert p '#' m

partial parseObstacle : String -> (Int, Int)
parseObstacle s = let (a::b::[]) = forget (split (==',') s) in (cast a, cast b)

add1024Obstacles : List (Int,Int) -> SortedMap (Int, Int) Char -> SortedMap (Int, Int) Char
add1024Obstacles l m = foldl addObstacle empty (take 1024 l)

partial part1 : String -> Int
part1 input =
    let obstacles = map parseObstacle (lines input)
        m = add1024Obstacles obstacles empty
        visitable = tail $ map (the Int 1000000000000000,) (concatMap (\y => map (,y) [the Int 0..70]) [the Int 0..70])
        shortestPath = dijkstra m ([(0,(0,0))] ++ visitable) (70,70) in shortestPath

-- Part 2

part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2