module Day12

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Data.Nat
import Utilities
import Data.SortedMap
import Data.SortedSet

-- Part 1

-- this is a fill/BFS

-- twoDStringToMap : String -> SortedMap (Int, Int) Char

neighbors : (Int, Int) -> List (Int, Int)
neighbors j = map (Utilities.(+) j) [(0,1),(0,-1),(1,0),(-1,0)]

validNeighbors : SortedMap (Int, Int) Char -> (Int, Int) -> List (Int, Int)
validNeighbors m pos = 
    let c' = lookup pos m in
        case c' of
            Just c => filter (\p => fromMaybe ' ' (lookup p m) == c) (neighbors pos)
            Nothing => []

-- dot = ignore

-- can't do a difference between maps unfortunately, would be a cool way to remove all visited nodes

-- returns (visited, new map)
floodFill : SortedMap (Int, Int) Char -> SortedSet (Int, Int) -> (Int, Int) -> SortedSet (Int, Int)
floodFill m visited pos = {-(trace $ show pos ++ " " ++ show (lookup pos m) ++ " " ++ show visited) $ -}if pos `contains` visited then empty else 
    let c' = lookup pos m in
        case c' of 
            Just c => let neighs = validNeighbors m pos in
                -- insert pos
                foldl (\currentVisited, neighbor => currentVisited `union` floodFill m (insert pos currentVisited) neighbor) (insert pos visited) neighs
            Nothing => empty

area : SortedSet (Int, Int) -> Int
area = cast . length . Data.SortedSet.toList

-- 0,0 down = 1,0 up
data Side = MkSide (Int, Int) (Int, Int)

-- instance Eq Side
--     (==) (MkSide pos dir) = 

-- issue: multiple sides on same untouched point
perimeter : SortedSet (Int, Int) -> Int
perimeter l =
    let boxAroundEachElement : List ((Int, Int), (Int, Int)) = concatMap (\pt => map (\neigh => (neigh,pt)) (neighbors pt)) l in (cast . length) $ Prelude.List.filter (\(dst,src) => not (dst `elem` l)) boxAroundEachElement
        -- allSides: SortedSet (Int, Int) = fromList boxAroundEachElement
        -- alreadyAccountedFor: SortedSet (Int, Int) = fromList l
        -- finalSet = difference allSides alreadyAccountedFor in 
        --     (trace $ show allSides ++ " " ++ show alreadyAccountedFor)
        --     (cast . length) (Data.SortedSet.toList finalSet)

regionScores : SortedMap (Int, Int) Char -> Int
regionScores m = if m == empty then 0 else
    let startingPoint = head @{believe_me (NonEmpty (keys m))} (keys m)
        region = floodFill m empty startingPoint
        score = (area region) * (perimeter region)
        removed = foldl (flip delete) m region
        next = regionScores removed in -- (trace $ "*****" ++ show removed)
            score + next

part1 : String -> Int
part1 = regionScores . twoDStringToMap

-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2