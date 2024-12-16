module Day16

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Data.Nat
import Utilities
import Data.SortedMap
import Data.SortedSet
import Data.List1
import MinHeap

-- Part 1

-- Starting at 12:44pm Dec 16, decided to sleep

{-
The Reindeer start on the Start Tile (marked S) facing East and need to reach the End Tile (marked E).
They can move forward one tile at a time (increasing their score by 1 point), but never into a wall (#).
They can also rotate clockwise or counterclockwise 90 degrees at a time (increasing their score by 1000 points).
 -}

-- So this is finding an optimal path, turning has a cost of 1000 points and forward has a cost of 1 point
-- Dijkstra/A*?
-- We want to minimize the number of turns

neighbors' : (Int, Int) -> List (Int, Int)
neighbors' j = map (Utilities.(+) j) [(0,1),(0,-1),(1,0),(-1,0)]

-- state will be stored as ((Int, Int), (Int, Int))
-- (position, direction)

turnRight : (Int, Int) -> (Int, Int)
-- ORDER IS (row, column) aka (y, x)
turnRight t = if t == (-1, 0) then (0, 1) -- up -> right
    else if t == (0, 1) then (1, 0) -- right -> down
    else if t == (1, 0) then (0, -1) -- down -> left
    else if t == (0, -1) then (-1, 0) -- left -> up
    else (0, 0)

turnLeft : (Int, Int) -> (Int, Int)
turnLeft t = (0, 0) - turnRight t

decreaseKey : BinaryHeap (Int,((Int, Int), (Int, Int))) -> (Int,((Int,Int),(Int,Int))) -> BinaryHeap (Int,((Int, Int), (Int, Int)))
decreaseKey heap (prio,val) = 
    case findIndex (\(_,v) => v == val) heap of
        (Just idx') =>
            let idx : Fin (length heap) = idx'
                currentPrio : Int = fst (index' heap idx)
                h : BinaryHeap (Int,((Int, Int), (Int, Int))) = (replaceWhen (\(_,v) => v == val) (min currentPrio prio, val) heap)
                upped = (heapifyUp h (finToNat idx) {p2=believe_me (InBounds (finToNat idx) h)}) in 
                -- (trace $ show h ++ "\n\n" ++ show upped) 
                upped
        _ => heap

neighbors : SortedMap (Int, Int) Char -> ((Int, Int), (Int, Int)) -> List (Int,((Int, Int), (Int, Int)))
neighbors m (pos,dir) = 
    let forward = (1,(pos + dir, dir))
        left = (1000,(pos, turnLeft dir))
        right = (1000,(pos, turnRight dir)) in filter (\(_,(p,_)) => '#' /= (fromMaybe '#' (lookup p m))) [forward, left, right] -- have to check if not wall (instead of being a dot) because that allows turning on start

-- ideally use a min-heap but this could be fast enough maybe
-- but...having 4 nodes for every point seems extremely inefficient...
partial dijkstra : SortedMap (Int, Int) Char -> BinaryHeap (Int,((Int, Int), (Int, Int))) -> (Int, Int) -> Int
dijkstra _ [] _ = 0
dijkstra m unvisited end =
    let (Just (distance,next)) = findMin unvisited in --(trace $ show (next,distance)) $
            if (fst next) == end then distance
            else
                let neighs : List (Int, ((Int, Int), (Int, Int))) = map (\(d,(pos,dir)) => (d+distance,(pos, dir))) (neighbors m next)
                    newUnvisited = foldl decreaseKey (deleteMin unvisited) neighs

                in 
                --(trace $ show (map fst newUnvisited) ++ "\n\n\n") $ 
                -- 0
                dijkstra m newUnvisited end

partial part1 : String -> Int
part1 input =
    let m = twoDStringToMap input
        l : List ((Int, Int), Char) = toList m
        start : ((Int, Int), (Int, Int)) = ((fst $ ne head $ (filter (\(k,v) => v == 'S') l)), (0,1))
        end = (fst $ ne head $ (filter (\(k,v) => v == 'E') l))
        visitable'' : List ((Int, Int), Char) = filter (\(k,v) => v /= '#') l
        visitable' : List ((Int, Int), (Int, Int)) = concatMap (\p => map (p,) [(the Int 0,the Int 1),(0,-1),(1,0),(-1,0)]) (map fst visitable'')
        visitable = map (1000000000000000,) visitable'
        h' = ([(0,start)] ++ visitable)
        h : BinaryHeap (Int,((Int, Int), (Int, Int))) = foldl insert [] h' in
            --(trace $ show h ++ "\n\n" ++ show (decreaseKey h (1,((1, 5), (-1, 0))))) $
            dijkstra m ([(0,start)] ++ visitable) end

-- Part 2

-- decreaseKey2 : Show a => Show p => Eq a => Eq p => Ord a => Ord p => BinaryHeap (Int,a,List p) -> (Int,a,List p) -> BinaryHeap (Int,a,List p)
-- decreaseKey2 heap (prio,val,path) = 
--     case findIndex (\(_,v,_) => v == val) heap of
--         (Just idx') =>
--             let idx : Fin (length heap) = idx'
--                 (currentPrio,_,currentPath) = index' heap idx
--                 h : BinaryHeap (Int,a,List p) = (replaceWhen (\(_,v,_) => v == val) (min currentPrio prio, val, if currentPrio == prio then path ++ currentPath else if min currentPrio prio == prio then path else currentPath) heap)
--                 upped = (heapifyUp h (finToNat idx) {p2=believe_me (InBounds (finToNat idx) h)}) in 
--                 -- (trace $ show h ++ "\n\n" ++ show upped) 
--                 upped
--         _ => heap

decreaseKey2 : (List (Ordering,(Int,Int)), BinaryHeap (Int,((Int, Int), (Int, Int)))) -> (Int,((Int,Int),(Int,Int))) -> (List (Ordering,(Int,Int)), BinaryHeap (Int,((Int, Int), (Int, Int))))
decreaseKey2 (l,heap) (prio,val) = 
    case findIndex (\(_,v) => v == val) heap of
        (Just idx') =>
            let idx : Fin (length heap) = idx'
                currentPrio : Int = fst (index' heap idx)
                h : BinaryHeap (Int,((Int, Int), (Int, Int))) = (replaceWhen (\(_,v) => v == val) (min currentPrio prio, val) heap)
                upped = (heapifyUp h (finToNat idx) {p2=believe_me (InBounds (finToNat idx) h)}) in 
                -- (trace $ show h ++ "\n\n" ++ show upped) 
                ((prio `compare` currentPrio, fst val) :: l, upped)
        _ => ((EQ,fst val) :: l, heap)

partial dijkstra2 : SortedMap (Int, Int) Char -> BinaryHeap (Int,((Int, Int), (Int, Int))) -> SortedMap (Int, Int) (List (SortedSet (Int, Int))) -> SortedMap (Int, Int) (List (SortedSet (Int, Int)))
dijkstra2 _ [] pathMap = pathMap
dijkstra2 m unvisited pathMap =
    let (Just (distance,next)) = findMin unvisited in --(trace $ show (next,distance)) $
        let pathsTakenHere : List (SortedSet (Int, Int)) = fromMaybe [] (lookup (fst next) pathMap)
            neighs : List (Int, ((Int, Int), (Int, Int))) = map (\(d,(pos,dir)) => (d+distance,(pos, dir))) (neighbors m next)
            bh :  BinaryHeap (Int,((Int,Int),(Int,Int))) = deleteMin unvisited
            s : (List (Ordering, (Int,Int)), BinaryHeap (Int,((Int,Int),(Int,Int)))) = ([],bh)
            (statuses, newUnvisited) = foldl decreaseKey2 s neighs
            betterPaths' = filter (\(a,b) => a == LT) statuses
            betterPaths = map snd betterPaths' -- these are all the neighbors where we found a better path
            additionalPaths' = filter (\(a,b) => a == EQ) statuses
            additionalPaths = map snd additionalPaths' -- these are all the neighbors where we found an equal path
            betterPathMap = concatMap (\n => map (\thisPath => (n,[Data.SortedSet.insert n thisPath])) pathsTakenHere) betterPaths
            additionalPathMap = concatMap (\n => map (\thisPath => (n,[Data.SortedSet.insert n thisPath])) pathsTakenHere) additionalPaths
            betterPathUpdatedMap = mergeLeft (fromList betterPathMap) pathMap
            additionalPathUpdatedMap = mergeWith (++) (fromList additionalPathMap) pathMap
        in 
        --(trace $ show (map fst newUnvisited) ++ "\n\n\n") $ 
        -- 0
        dijkstra2 m newUnvisited pathMap

partial part2 : String -> Int
part2 input =
    let m = twoDStringToMap input
        l : List ((Int, Int), Char) = toList m
        start : ((Int, Int), (Int, Int)) = ((fst $ ne head $ (filter (\(k,v) => v == 'S') l)), (0,1))
        end = (fst $ ne head $ (filter (\(k,v) => v == 'E') l))
        visitable'' : List ((Int, Int), Char) = filter (\(k,v) => v /= '#') l
        visitable' : List ((Int, Int), (Int, Int)) = concatMap (\p => map (p,) [(the Int 0,the Int 1),(0,-1),(1,0),(-1,0)]) (map fst visitable'')
        visitable = map (1000000000000000,) visitable'
        h' = ([(0,start)] ++ visitable)
        h : BinaryHeap (Int,((Int, Int), (Int, Int))) = foldl insert [] h'
        pathMap = dijkstra2 m ([(0,start)] ++ visitable) empty in (trace $ show (lookup end pathMap)) 2
            --(trace $ show h ++ "\n\n" ++ show (decreaseKey h (1,((1, 5), (-1, 0))))) $
            

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2