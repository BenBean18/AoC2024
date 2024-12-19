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

zeroTo70 : List Int
zeroTo70 = [0..70]

partial part1 : String -> Int
part1 input =
    let obstacles = map parseObstacle (lines input)
        m = add1024Obstacles obstacles empty
        visitable = tail $ map (the Int 100000000,) (concatMap (\y => map (,y) zeroTo70) zeroTo70)
        shortestPath = dijkstra m ([(0,(0,0))] ++ visitable) (70,70) in shortestPath

-- Part 2

-- we can binary search on the number of obstacles
-- LETS GO THIS WILL BE FUN

-- 1024-3000
-- so (1024+3000)/2 = 2012 is next pick
-- if unblocked and next is blocked, then the next obstacle's coordinates
-- if unblocked and not, go higher with 2012-3000
-- if blocked and previous is unblocked, then the current obstacle's coordinates
-- if blocked and not, go lower with 1024-2011

addNObstacles : List (Int,Int) -> SortedMap (Int, Int) Char -> Nat -> SortedMap (Int, Int) Char
addNObstacles l m n = foldl addObstacle empty (take n l)

partial binarySearch : List (Int,Int) -> SortedMap (Int, Int) Char -> BinaryHeap (Int,(Int,Int)) -> Int -> Int -> (Int,Int)
binarySearch l m visitable lower upper =
    let n = (trace $ show lower ++ "-" ++ show upper) $ (lower + upper) `div` 2
        prevBlocked = (dijkstra (addNObstacles l m ((cast n) `minus` 1)) visitable (70,70)) == 100000000
        currentObstacle = (ne last) (take (cast n) l)
        blocked = (dijkstra (addNObstacles l m (cast n)) visitable (70,70)) == 100000000
        nextObstacle = (ne last) (take ((cast n)+1) l)
        nextBlocked = (dijkstra (addNObstacles l m ((cast n)+1)) visitable (70,70)) == 100000000 in
            if blocked && not prevBlocked then currentObstacle
            else if blocked then binarySearch l m visitable lower n
            else if not blocked && nextBlocked then nextObstacle
            else if not blocked && not nextBlocked then binarySearch l m visitable n upper
            else binarySearch l m visitable n upper -- shouldn't need this?

partial part2 : String -> Int
part2 input =
    let obstacles = map parseObstacle (lines input)
        visitable = tail $ map (the Int 100000000,) (concatMap (\y => map (,y) zeroTo70) zeroTo70)
        (x,y) = binarySearch obstacles empty ([(0,(0,0))] ++ visitable) 1024 (cast (length obstacles)) in (trace $ show x ++ "," ++ show y) 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2