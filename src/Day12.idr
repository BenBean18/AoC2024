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
findLines (a :: b :: xs) = if magnitude (a - b) > 1 || (fst a /= fst b) || (abs (snd a - snd b) > 1) then 1 + findLines (b :: xs) else findLines (b :: xs)
findLines [_] = 1
findLines [] = 0

listDifference : Eq a => Ord a => List a -> List a -> List a
listDifference a b = foldl (flip delete) a b

highlight : List Char -> List Char
highlight s = unpack ("\x1b" ++ "[43m" ++ (pack s) ++ "\x1b" ++ "[0m")

renderPath : SortedMap (Int, Int) Char -> List (Int, Int) -> String
renderPath m path = --"\x1b" ++ "c" ++
    let s = sort (keys m)
        (maxY, maxX) = ne last s
        (minY, minX) = ne head s
        toRender = map (\y => pack $ concatMap (\x => 
            let str: List Char = [(fromMaybe ' ') (lookup (cast y,cast x) m)] in
                if (y,x) `elem` path then highlight str else str) [minX..maxX]) [minY..maxY] in unlines toRender


numSides : SortedMap (Int, Int) Char -> SortedSet (Int, Int) -> Int
numSides m l =
    let boxAroundEachElement : List ((Int, Int), (Int, Int)) = concatMap (\pt => map (\neigh => (neigh,pt)) (neighbors pt)) l
        outerSides' : List ((Int, Int), (Int, Int)) = Prelude.List.filter (\(dst,src) => not (dst `elem` l)) boxAroundEachElement
        -- default sort sorts by first coordinate which is y
        -- so we find all the sides that are vertical
        -- i.e. the dest is horizontal from the source and their vert coords are the same i think?

        -- vertical: sort normally
        -- horizontal: sort inverted
        verticalBorderToTheLeft : List (Int, Int) = sort $ map (\(a,b)=>(b,a)) $ map (\(d,s)=>d) $ filter (\(d,s) => (d - s) == (the Int 0,the Int (-1))) outerSides'
        verticalBorderToTheRight : List (Int, Int) = sort $ map (\(a,b)=>(b,a)) $ map (\(d,s)=>d) $ filter (\(d,s) => (d - s) == (the Int 0,the Int 1)) outerSides'
        horizontalBorderToTheUp : List (Int, Int) = sort $ map (\(d,s)=>d) $ filter (\(d,s) => (d - s) == (the Int (-1),the Int 0)) outerSides'
        horizontalBorderToTheDown : List (Int, Int) = sort $ map (\(d,s)=>d) $ filter (\(d,s) => (d - s) == (the Int 1,the Int 0)) outerSides'
        -- border : List (Int, Int) = sort $ map (\(a,b)=>(b,a)) $ map (\(d,s)=>d) $ filter (\((dy,dx),(sy,sx)) => dy == sy) outerSides'
        -- border' = sort $ map (\(d,s)=>d) $ filter (\((dy,dx),(sy,sx)) => dx == sx) outerSides'
        -- lines = findLines (nub border) + findLines (listDifference border (nub border))
        -- lines' = findLines (nub border') + findLines (listDifference border' (nub border')) in
        --     (trace $ "lines=" ++ show lines ++ " lines'=" ++ show lines' ++
        --     "\n\nb=" ++ (show (findLines (nub border))) ++ "\n" ++ (renderPath m (map (\(a,b)=>(b,a)) border)) ++
        --     "\n\ndedup=" ++ (show (findLines (listDifference border (nub border)))) ++ "\n" ++ (renderPath m (map (\(a,b)=>(b,a)) (listDifference border (nub border)))) ++
        --     "\n\nb'=" ++ (show (findLines (nub border'))) ++ "\n" ++ (renderPath m border') ++
        --     "\n\ndedup=" ++ (show (findLines (listDifference border' (nub border')))) ++ "\n" ++ (renderPath m (listDifference border' (nub border')))) $ 
        --     lines + lines' -- sorting is important
        in sum (map findLines [verticalBorderToTheLeft,verticalBorderToTheRight,horizontalBorderToTheUp,horizontalBorderToTheDown])

regionScores2 : SortedMap (Int, Int) Char -> Int
regionScores2 m = if m == empty then 0 else
    let startingPoint = head @{believe_me (NonEmpty (keys m))} (keys m)
        region = floodFill m empty startingPoint
        a = (area region)
        ns = (numSides m region)
        score = a * ns
        removed = foldl (flip delete) m region
        next = regionScores2 removed in -- (trace $ "*****" ++ show removed)
            (trace $ show (lookup startingPoint m) ++ " " ++ show a ++ " " ++ show ns ++ "\n") $ score + next

-- 891692 too low

-- https://www.reddit.com/r/adventofcode/comments/1hcfurk/2024_day_12_another_test_case/ breaks mine! I get 868

part2 : String -> Int
part2 = regionScores2 . twoDStringToMap

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2