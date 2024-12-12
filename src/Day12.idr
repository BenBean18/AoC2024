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
                -- (insert pos visited) not visited is VERY important
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

-- findStraightLine : List (Int, Int) -> (Int, Int) -> List (Int, Int)
-- findStraightLine (x :: xs) dir =
--     let next = x + dir
--         next' = x - dir in 
--         if next `elem` xs then (trace $ show x ++ " " ++ show dir) $ x :: next :: findStraightLine (next :: xs) dir
--         else if next' `elem` xs then (trace $ show x ++ " " ++ show dir) $ x :: next' :: findStraightLine (next' :: xs) dir
--         else []
-- findStraightLine [] _ = (trace "done") []

-- finding a straight line in a list of coordinates, going in a specific direction or backwards
-- start at first element of list
-- check to see if + or - are in the list
-- if they are, add them (along with the current node) to the path list, remove them from the remaining list, and recurse on the remaining list
-- if they're not, nothing there
findStraightLine : List (Int, Int) -> (Int, Int) -> List (Int, Int)
findStraightLine (x :: xs) dir = {-(trace $ show x ++ " " ++ show dir) $-}
    let next = x + dir
        next' = x - dir in 
        if next `elem` xs then {-(trace $ show x ++ " " ++ show dir) $-} x :: next :: findStraightLine (next :: (delete next xs)) dir
        else if next' `elem` xs then {-(trace $ show x ++ " " ++ show dir) $-} x :: next' :: findStraightLine (next' :: (delete next' xs)) dir
        else findStraightLine xs dir
findStraightLine [] _ = (trace "done") []

-- :exec printLn (nub (findLines (sort [(0,0),(0,1),(0,2),(1,1),(5,1),(0,3)])))

findStraightLines : List (Int, Int) -> Int
findStraightLines [] = 0
findStraightLines l = (trace $ "hi" ++ show l) $ 
    let nextLines : List (List (Int, Int)) = filter (\xs => length xs /= 0) $ map (\a => nub (findStraightLine l a)) [(0,1),(1,0)]
        visited : List (Int, Int) = concat nextLines
        visitedSet : SortedSet (Int, Int) = fromList visited
        lSet : SortedSet (Int, Int) = fromList l
        nextList = Data.SortedSet.toList (lSet `difference` visitedSet) in (trace $ show visitedSet ++ " " ++ show lSet) ((cast . length) nextLines) + findStraightLines (Data.SortedSet.toList (lSet `difference` visitedSet))

-- better way possibly
-- sort on first coordinate to get that direction ones
-- sort on second coordinate to get that direction ones

magnitude : (Int, Int) -> Int
magnitude (a,b) = a*a + b*b

-- expects sorted list by first then second coordinate
-- wait that's it
-- we sort the list by first and second coordinate and look for a magnitude of the jump > 1

-- (-1, 0), (-1, 1), (-1, 2), (-1, 3), (-1, 4), (-1, 5), **(0, -1), (0, 6)**is a problem

findLines : List (Int, Int) -> Int
findLines (a :: b :: xs) = if (fst a) /= (fst b) then {-(trace $ show a ++ " " ++ show b) $ -}1 + findLines (b :: xs) else findLines (b :: xs)
findLines _ = 1 -- only adding one during transition, so start at one

numSides : SortedSet (Int, Int) -> Int
numSides l =
    let boxAroundEachElement : List ((Int, Int), (Int, Int)) = concatMap (\pt => map (\neigh => (neigh,pt)) (neighbors pt)) l
        outerSides' : List ((Int, Int), (Int, Int)) = Prelude.List.filter (\(dst,src) => not (dst `elem` l)) boxAroundEachElement
        border : List (Int, Int) = sort $ map fst outerSides'
        border' = sort $ (map (\(a,b)=>(b,a)) border)
        lines = findLines border
        lines' = findLines border' in (trace $ "lines=" ++ show lines ++ "lines'=" ++ show lines' ++ "\n\nb=" ++ show border ++ "\n\nb'=") $ lines + lines' -- sorting is important

regionScores2 : SortedMap (Int, Int) Char -> Int
regionScores2 m = if m == empty then 0 else
    let startingPoint = head @{believe_me (NonEmpty (keys m))} (keys m)
        region = floodFill m empty startingPoint
        score = (area region) * (numSides region)
        removed = foldl (flip delete) m region
        next = regionScores removed in -- (trace $ "*****" ++ show removed)
            score + next

part2 : String -> Int
part2 = regionScores2 . twoDStringToMap

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2