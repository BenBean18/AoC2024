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

neighbors : SortedMap (Int, Int) Char -> ((Int, Int), (Int, Int)) -> List (((Int, Int), (Int, Int)), (Int, Bool))
neighbors m (pos,dir) = 
    let forward = ((pos + dir, dir), (1, False))
        left = ((pos, turnLeft dir), (1000, False))
        right = ((pos, turnRight dir), (1000, False)) in filter (\((p,_),_) => '#' /= (fromMaybe '#' (lookup p m))) [forward, left, right] -- have to check if not wall (instead of being a dot) because that allows turning on start

-- ideally use a min-heap but this could be fast enough maybe
-- but...having 4 nodes for every point seems extremely inefficient...
dijkstra : SortedMap (Int, Int) Char -> SortedMap ((Int, Int), (Int, Int)) (Int, Bool) -> (Int, Int) -> Int
dijkstra m distanceAndVisitedMap end =
    let l: List (((Int, Int), (Int, Int)), (Int, Bool)) = filter (\((_,_),(_,visited)) => not visited) $ Data.SortedMap.toList distanceAndVisitedMap
        (next,(distance,_)) = (ne head) (sortBy (compare `on` (\((_,_),(distance,_)) => distance)) l) in --(trace $ show (next,distance)) $
            if (fst next) == end then distance
            else
                let neighs : SortedMap ((Int, Int), (Int, Int)) (Int, Bool) = fromList $ filter (\(_,(_,v)) => not v) $ map (\((pos,dir),(d,v)) => ((pos, dir),(d + distance,v))) (neighbors m next)
                    newMap = mergeWith (\(d1,v1),(d2,v2) => (min d1 d2, v1 || v2)) neighs (insert next (distance, True) distanceAndVisitedMap)
                in {-(trace $ show neighs) $ -}dijkstra m newMap end

part1 : String -> Int
part1 input =
    let m = twoDStringToMap input
        l : List ((Int, Int), Char) = toList m
        start : ((Int, Int), (Int, Int)) = ((fst $ ne head $ (filter (\(k,v) => v == 'S') l)), (0,1))
        end = (fst $ ne head $ (filter (\(k,v) => v == 'E') l))
        visitable'' : List ((Int, Int), Char) = filter (\(k,v) => v /= '#') l
        visitable' : List ((Int, Int), (Int, Int)) = concatMap (\p => map (p,) [(the Int 0,the Int 1),(0,-1),(1,0),(-1,0)]) (map fst visitable'')
        visitable = SortedMap.fromList $ map (,(1000000000000000,False)) visitable' in
            dijkstra m (mergeLeft (fromList [(start,(0,False))]) visitable) end
-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2