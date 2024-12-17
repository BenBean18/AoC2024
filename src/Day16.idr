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

decreaseKey2 : (List ((Ordering,Int),(Int,Int)), BinaryHeap (Int,((Int, Int), (Int, Int)))) -> (Int,((Int,Int),(Int,Int))) -> (List ((Ordering,Int),(Int,Int)), BinaryHeap (Int,((Int, Int), (Int, Int))))
decreaseKey2 (l,heap) (prio,val) = 
    case findIndex (\(_,v) => v == val) heap of
        (Just idx') =>
            let idx : Fin (length heap) = idx'
                currentPrio : Int = fst (index' heap idx)
                h : BinaryHeap (Int,((Int, Int), (Int, Int))) = (replaceWhen (\(_,v) => v == val) (min currentPrio prio, val) heap)
                upped = (heapifyUp h (finToNat idx) {p2=believe_me (InBounds (finToNat idx) h)}) in 
                -- (trace $ show h ++ "\n\n" ++ show upped) 
                (((prio `compare` currentPrio,currentPrio), fst val) :: l, upped)
        _ => (((EQ,0),fst val) :: l, heap)

highlight : List Char -> List Char
highlight s = ['O']--unpack ("\x1b" ++ "[43m" ++ (pack s) ++ "\x1b" ++ "[0m")

renderPath : SortedMap (Int, Int) Char -> List (Int, Int) -> String
renderPath m path = --"\x1b" ++ "c" ++
    let s = sort (keys m)
        (maxY, maxX) = ne last s
        (minY, minX) = ne head s
        toRender = map (\y => pack $ concatMap (\x => 
            let str: List Char = [(fromMaybe ' ') (lookup (cast y,cast x) m)] in
                if (y,x) == (ne head) path then ['X'] else if (y,x) `elem` path then highlight str else str) [minX..maxX]) [minY..maxY] in unlines toRender

{-
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#OOO#.#.#.....#.#
#O#O#.#.#.#####.#
#O#O..#.#.#....X#
#O#O#####.#.###O#
#O#O#.......#OOO#
#O#O###.#####O###
#O#O#...#..OOO#.#
#O#O#.#####O###.#
#O#O#OOOOOOO..#.#
#O#O#O#########.#
#S#OOO..........#
#################
not continuing upward because we've already reached that vertex, which seems wrong
we should at least add it to the list of paths

11042
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#OOO#.#.#.....#.#
#O#O#.#.#.#####.#
#O#O..#.#.#....X#
#O#O#####.#.###O#
#O#O#.......#OOO#
#O#O###.#####O###
#O#O#...#..OOO#.#
#O#O#.#####O###.#
#O#O#OOOOOOO..#.#
#O#O#O#########.#
#S#OOO..........#
#################

11042
#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#OOO#.#.#.....#.#
#O#O#.#.#.#####.#
#O#O..#.#.#OOOOX#
#O#O#####.#O###.#
#O#O#..OOOOO#...#
#O#O###O#####.###
#O#O#OOO#.....#.#
#O#O#O#####.###.#
#O#O#O........#.#
#O#O#O#########.#
#S#OOO..........#
#################

they're equal!!
 -}

-- every time we're at a node,
-- get a list of the paths to neighbors that are either better than or equal to the current solution.
-- to turn into a map, we do (neighbor aka destination, [path to neighbor]) for every one
-- if better, then erase the current list of paths to that vertex and replace with the better paths


partial dijkstra2 : SortedMap (Int, Int) Char -> BinaryHeap (Int,((Int, Int), (Int, Int))) -> SortedMap (Int, Int) (List (List (Int, Int))) -> SortedMap (Int, Int) (List (List (Int, Int)))
dijkstra2 _ [] pathMap = pathMap
dijkstra2 m unvisited pathMap =
    let (Just (distance,next)) = findMin unvisited in (trace $ show (next,distance)) $
        if (fst next) == (1,15) then pathMap else
        if distance > 11048 then pathMap else
        let pathsTakenHere : List (List (Int, Int)) = fromMaybe [[fst next]] (lookup (fst next) pathMap)
            neighs : List (Int, ((Int, Int), (Int, Int))) = map (\(d,(pos,dir)) => (d+distance,(pos, dir))) (neighbors m next)
            bh :  BinaryHeap (Int,((Int,Int),(Int,Int))) = deleteMin unvisited
            s : (List ((Ordering,Int), (Int,Int)), BinaryHeap (Int,((Int,Int),(Int,Int)))) = ([],bh)
            (statuses, newUnvisited) = foldl decreaseKey2 s neighs
            betterPaths' = filter (\((a,_),b) => a == LT) statuses
            betterPaths = map snd betterPaths' -- these are all the neighbors where we found a better path
            additionalPaths' = filter (\((a,_),b) => a == EQ) statuses
            additionalPaths = map snd additionalPaths' -- these are all the neighbors where we found an equal path
            betterPathMaps : List (SortedMap (Int, Int) (List (List (Int, Int)))) = concatMap (\n => map (\thisPath => 
                let currentPaths = fromMaybe [] (lookup n pathMap)
                    firstPath = if (isNil currentPaths) then "" else renderPath m (ne head (currentPaths)) in
                --(trace $ show distance
                -- ++ " - " ++ show n ++ " - " ++ show (length currentPaths) ++ " - best cost - " ++ show statuses ++ " - best: \n" ++ firstPath ++ "\n" ++ (renderPath m (n::thisPath))
                --) $ 
                (singleton n [n::thisPath])) pathsTakenHere) betterPaths
            betterPathMap = foldl (mergeWith (++)) empty betterPathMaps
            additionalPathMaps = concatMap (\n => map (\thisPath => 
                --(trace $ show distance ++ "\n ***" ++ (renderPath m thisPath)) $ 
                (singleton n [n::thisPath])) pathsTakenHere) additionalPaths
            additionalPathMap = foldl (mergeWith (++)) empty additionalPathMaps
            additionalPathUpdatedMap = mergeWith (++) additionalPathMap pathMap
            betterPathUpdatedMap = mergeLeft betterPathMap additionalPathUpdatedMap
        in 
        (trace $ show distance ++ "\n") $ 
        -- 0
        dijkstra2 m newUnvisited betterPathUpdatedMap















-- Node type: ((Int, Int), (Int, Int))
-- Arguments:
-- - m: character map for rendering/neighbor finding
-- - unvisited: heap/priority queue, entries are (cost, unvisited vertex)
-- - previous: key is vertex, value is (lowest cost to the vertex, all previous vertices that result in that cost)
-- once heap is empty, we can return previous
-- and then traverse the tree, backtracking from the end, then put that into a set and get the number of unique values
partial dijkstraNew : SortedMap (Int, Int) Char -> BinaryHeap (Int,((Int, Int), (Int, Int))) -> SortedMap ((Int,Int),(Int,Int)) (Int, List ((Int,Int),(Int,Int))) -> SortedMap ((Int,Int),(Int,Int)) (Int, List ((Int,Int),(Int,Int)))
dijkstraNew m [] previous = previous
dijkstraNew m unvisited previous =
    let (Just (distance,next)) = findMin unvisited
        neighs : List (Int, ((Int, Int), (Int, Int))) = map (\(d,(pos,dir)) => (d+distance,(pos, dir))) (neighbors m next) -- all neighbors and their costs
        newUnvisited = foldl decreaseKey (deleteMin unvisited) neighs -- updated priority queue (if cost is lower, update cost)
        -- update the previous vertex map. for every neighbor: 
        -- - if the cost is lower than the cost in the previous vertex map, set the list of previous vertices to just be this vertex
        -- - if the cost is equal, add this vertex to the list of previous vertices
        -- - if the cost is more, do nothing
        newPrevious = foldl (\currentMap, (cost,neighbor) =>
            let (currentCost, currentPreviousVertices) : (Int, List ((Int,Int),(Int,Int))) = fromMaybe (1000000000000000,[]) (lookup neighbor currentMap) in
                --(trace $ show currentCost) $
                if cost < currentCost then (insert neighbor (cost, [next]) currentMap)
                else if cost == currentCost then (insert neighbor (cost, next :: currentPreviousVertices) currentMap)
                else currentMap) previous neighs
        in --(trace $ show next ++ " " ++ show (length unvisited)) $
            dijkstraNew m newUnvisited newPrevious

-- does not check for uniqueness yet
backtrack : SortedMap ((Int,Int),(Int,Int)) (Int, List ((Int,Int),(Int,Int))) -> ((Int,Int),(Int,Int)) -> List ((Int,Int),(Int,Int))
backtrack previous current = --(trace $ show current) $
    let (_,previousVertices) = fromMaybe (1000000000000000,[]) (lookup current previous) in-- (trace $ show previousVertices) $
        --if isNil previousVertices then [] else
        concatMap (\vertex => vertex :: backtrack previous vertex) previousVertices






















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
        previousMap = dijkstraNew m ([(0,start)] ++ visitable) (singleton start (0,[]))
        allSquares = nub $ map fst $ backtrack previousMap (end,(1,0)) in 
            -- (trace $ show end) 2
            (trace $ renderPath m allSquares ++ " " ++ show (length allSquares)) 2
            --(trace $ show h ++ "\n\n" ++ show (decreaseKey h (1,((1, 5), (-1, 0))))) $

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2