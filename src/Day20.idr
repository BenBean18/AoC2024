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

-- wait this is like pathing along the wall
-- as well as through the track
-- seems like memoization helps here? idk
-- there appears to be only one path (of length ~9500 on real input) through the entire racetrack
-- so we need to consider all paths of length 102 (cheat of length 2)-120 (cheat of length 20)
-- if it's 7 long and we want paths of 3, there are abcdefg -> abc,bcd,cde,def,efg. so (length of entire thing) - (length of path) + 1 combos
-- 188000 possible cheats to check........checking 13972 took 2.5 minutes.
-- except it's not as bad since we only need to check the length of the wall path
-- and really, "cheating" is just ignoring obstacles/all moves within the map are valid
-- remember: same start and end = same cheat
-- store as (time saved, (start, end)) in a set

bfsPath : SortedMap (Int, Int) Char -> List (Int, Int) -> (Int, Int) -> (Int, Int) -> List (Int,Int)
bfsPath m visited current end = 
    if current == end then (visited++[end]) -- oooooops i kinda forgot to add the end so was missing cheats :(
    else if current `elem` visited then [] else
    concatMap (\n => bfsPath m (visited++[current]) n end) (neighbors m current) -- only one path so this is fine

findPath : SortedMap (Int, Int) Char -> List (Int, Int)
findPath m =
    let l : List ((Int, Int), Char) = toList m
        start : (Int,Int) = (fst $ ne head $ (filter (\(k,v) => v == 'S') l))
        end : (Int,Int) = (fst $ ne head $ (filter (\(k,v) => v == 'E') l)) in bfsPath m [] start end

highlight : List Char -> List Char
highlight s = unpack ("\x1b" ++ "[43mO\x1b" ++ "[0m")

renderPath : SortedMap (Int, Int) Char -> List (Int, Int) -> String
renderPath m path = --"\x1b" ++ "c" ++
    let s = sort (keys m)
        (maxY, maxX) = ne last s
        (minY, minX) = ne head s
        toRender = map (\y => pack $ concatMap (\x => 
            let str: List Char = [(fromMaybe ' ') (lookup (cast y,cast x) m)] in
                if (y,x) == (ne head) path then ['X'] else if (y,x) `elem` path then highlight str else str) [minX..maxX]) [minY..maxY] in unlines toRender

pathsOfLength : List (Int, Int) -> Nat -> List ((Int,Int),(Int,Int))
pathsOfLength l n = if (length l) < n then [] else
    let path' = take n l
        start = (ne head) path'
        path = (ne tail) l
        end = (ne last) path' in (start,end) :: pathsOfLength path n

manhattanDist : (Int, Int) -> (Int, Int) -> Int
manhattanDist (y1,x1) (y2,x2) = (abs (y2-y1)) + (abs (x2-x1))

timeSaved : SortedMap (Int, Int) Char -> List (Int, Int) -> Nat -> Int
timeSaved m path n = (trace $ show n) $ cast $ length $ filter (>=the Int 100) (map (\(start, end) => (cast n) - (manhattanDist start end) - 1) (pathsOfLength path n))

-- wait checking from start to end doesn't work
-- because a path of length 25 from start to end could involve 5 along the actual path at the end for example
-- better idea (in talking) could be to go along the path and look for all other dots within range, then lookup indices to see what the "normal" distance is compared to how much saved

within20 : (Int, Int) -> List (Int, (Int, Int))
within20 pos = map (\a => (manhattanDist (0,0) a, pos + a)) (filter (\a => manhattanDist (0,0) a <= 20) [(x,y) | x <- [-20..20], y <- [-20..20]])

partial findCheats2 : SortedMap (Int, Int) Int -> (Int, Int) -> Int
findCheats2 indexMap pos = (trace $ show pos) $
    let (Just start) = lookup pos indexMap
        toCheck : List (Int, (Int, Int)) = within20 pos
        times = map (\(dist,pos) =>
            case pos `lookup` indexMap of
                Just idx => (idx - start) - dist -- hopefully only + is ok? i think forward + backward was what was causing double counting before
                Nothing => 0) toCheck in cast (length (filter (>=the Int 100) times))

partial part2 : String -> Int
part2 input = 
    let m = twoDStringToMap input
        l : List ((Int, Int), Char) = toList m
        path = findPath m
        indexMap : SortedMap (Int, Int) Int = fromList (zip path (map cast [0..(length path `minus` 1)]))
        -- goodCheats = sum (map (timeSaved m path) [1..(length path)]) in goodCheats -- doesn't work, gives an answer ~42x too high
        goodCheats : Int = foldl (\acc, elem => acc + findCheats2 indexMap elem) 0 path in goodCheats

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2