module Day10

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.SortedMap
import Data.SortedSet
import System

-- Part 1: 4708us

-- For all practical purposes, this means that a hiking trail is any path that starts at height 0, ends at height 9, and always increases by a height of exactly 1 at each step. 
-- Hiking trails never include diagonal steps - only up, down, left, or right (from the perspective of the map).
-- Trailhead is zero.
-- Score is number of 9s reachable.

neighbors' : (Int, Int) -> List (Int, Int)
neighbors' j = map (Utilities.(+) j) [(0,1),(0,-1),(1,0),(-1,0)]

-- BOOM
-- I realized iterating through the entire map every time is very inefficient (436194us)
-- Old version:
-- let pos = neighbors' c
--     listForm = toList m in filter (\(k,v) => k `elem` pos) listForm

-- So I changed it to a lookup (which, y'know, is kinda what maps are for) and got 4708us
-- That's a 100x speedup
neighbors : SortedMap (Int, Int) Char -> (Int, Int) -> List ((Int, Int), Char)
neighbors m c =
    let pos = neighbors' c in map (\(a,b) => (a, fromMaybe '0' b)) $ filter (isJust . snd) $ map (\k => (k, lookup k m)) pos

bfs : SortedMap (Int, Int) Char -> ((Int, Int), Char) -> List ((Int, Int), Char)
bfs m (pos,char) =
    let neighs = neighbors m pos
        validNeighbors = filter (\(k,v) => (the (Char -> Int) cast) v == (1 + (the (Char -> Int) cast) char)) neighs in
            (pos,char) :: concatMap (bfs m) validNeighbors

-- the only thing that matters is the number of 9s reachable starting at a 0

ninesReachable : SortedMap (Int, Int) Char -> ((Int, Int), Char) -> List ((Int, Int), Char)
ninesReachable m s = nub $ filter (\(pos,char) => char == '9') (bfs m s)

findStarts : SortedMap (Int, Int) Char -> List ((Int, Int), Char)
findStarts m = filter (\(pos,char) => char == '0') (toList m)

example : SortedMap (Int, Int) Char
example = twoDStringToMap "0123\n1234\n8765\n9876"

part1 : String -> Int
part1 input =
    let m = twoDStringToMap input
        starts = findStarts m
        nines = concatMap (ninesReachable m) starts in cast $ length nines

-- Part 2: 4840us

highlight : List Char -> List Char
highlight s = unpack ("\x1b" ++ "[43m" ++ (pack s) ++ "\x1b" ++ "[0m")

renderPath : SortedMap (Int, Int) Char -> List ((Int, Int), Char) -> String
renderPath m path = "\x1b" ++ "c" ++
    let s = sort (keys m)
        (maxY, maxX) = ne last s
        (minY, minX) = ne head s
        toRender = map (\y => pack $ concatMap (\x => 
            let str: List Char = [(fromMaybe ' ') (lookup (cast y,cast x) m)] in
                if (y,x) `elem` (map fst path) then highlight str else str) [minX..maxX]) [minY..maxY] in unlines toRender

-- now we care about keeping track of every path, add it to the list when we hit a nine
bfsTracing' : SortedMap (Int, Int) Char -> List ((Int, Int), Char) -> ((Int, Int), Char) -> List (List ((Int, Int), Char))
bfsTracing' m visited (pos,char) = (trace $ renderPath m visited) $ unsafePerformIO (do
    usleep 100000
    let neighs = neighbors m pos
    let validNeighbors = filter (\(k,v) => (the (Char -> Int) cast) v == (1 + (the (Char -> Int) cast) char)) neighs
    pure $ nub (if char == '9' then [visited] else []) ++ concatMap (bfsTracing' m ((pos,char) :: visited)) validNeighbors)

part2 : String -> IO Int
part2 input =
    let m = twoDStringToMap input
        starts = findStarts m
        paths = concatMap (bfsTracing' m []) starts in do
            putStrLn (render2DMap m)
            pure (cast $ length paths)

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = part2