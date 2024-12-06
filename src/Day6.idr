module Day6

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.SortedMap

-- Part 1: runs in 11.773ms

mapIndexed' : Nat -> List a -> ((Nat, a) -> b) -> List b
mapIndexed' idx (x::xs) f = f (idx, x) :: mapIndexed' (S idx) xs f
mapIndexed' _ [] _ = []

mapIndexed : List a -> ((Nat, a) -> b) -> List b
mapIndexed = mapIndexed' 0

parseString' : Nat -> List (List Char) -> SortedMap (Int, Int) Char
parseString' rowIdx (row :: rows) = foldl
    (\m, ((r, c), e) => insert (r, c) e m) -- acc -> elem -> acc
    (parseString' (S rowIdx) rows) -- acc
    (mapIndexed row (\(idx, el) => ((((the (Integer -> Int) cast) . natToInteger) rowIdx, ((the (Integer -> Int) cast) . natToInteger) idx), el))) -- List elem
parseString' _ [] = empty

parseString : String -> SortedMap (Int, Int) Char
parseString l = parseString' Z (map unpack (lines l))

turnRight : (Int, Int) -> (Int, Int)
-- ORDER IS (row, column) aka (y, x)
turnRight t = if t == (-1, 0) then (0, 1) -- up -> right
    else if t == (0, 1) then (1, 0) -- right -> down
    else if t == (1, 0) then (0, -1) -- down -> left
    else if t == (0, -1) then (-1, 0) -- left -> up
    else (0, 0)

-- https://github.com/BenBean18/AoC2023/blob/0cfae9c97def9a82df5a849b37c2fc737ece2662/src/Day23.hs#L37C1-L42C22
directionOf : Char -> (Int, Int)
directionOf '>' = (0,1)
directionOf '<' = (0,-1)
directionOf 'v' = (1,0)
directionOf '^' = (-1,0)
directionOf _ = (0,0)

s : (Int, Int) -> (Int, Int) -> (Int, Int)
s (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

move : ((Int, Int), (Int, Int)) -> SortedMap (Int, Int) Char -> SortedMap (Int, Int) Char
move ((r, c), dir) m =
    let possible = {-(trace $ show (r,c)) $ -}(r, c) `s` dir in
            case (lookup possible m) of
                Just ch => {-(trace "obs") $ -}if ch == '#' then move ((r, c), (turnRight dir)) m
                    else {-(trace "mov") $ -}move (possible, dir) (insert (r,c) 'X' m)
                Nothing => (insert (r,c) 'X' m) -- returning just the map gives off by one error :(

findStart : SortedMap (Int, Int) Char -> ((Int, Int), (Int, Int))
findStart m =
    case filter (\k => ((fromMaybe '.' (lookup k m)) /= '.') && ((fromMaybe '.' (lookup k m)) /= '#')) (keys m) of
        (s :: []) => (s, (directionOf (fromMaybe '.' (lookup s m))))
        _ => ((0,0), (0,0))

numSquares : SortedMap (Int, Int) Char -> Int
numSquares m = cast (length (filter (== 'X') (values m)))

part1 : String -> Int
part1 input =
    let inputMap = parseString input
        st = findStart inputMap
        result = move st inputMap
        sq = numSquares result in sq

-- Part 2

-- I thought the condition was it had to get back to the start, but cycles exist that don't involve the starting vertex
-- Which is why I needed the 10000 iteration exit condition
-- Ugh

directionOf' : (Int, Int) -> Char
directionOf' (0,1) = '>' 
directionOf' (0,-1) = '<'
directionOf' (1,0) = 'v'
directionOf' (-1,0) = '^'
directionOf' _ = 'X'

-- brute force should be about 200 seconds, not ideal but i'm tired
-- good news! it only took ~51 (once I fixed it)!
isLoop : Bool -> ((Int, Int), (Int, Int)) -> SortedMap (Int, Int) Char -> Bool
isLoop isFirst ((r, c), dir) m = if directionOf (fromMaybe '.' (lookup (r,c) m)) == dir && not isFirst then {-(trace "true")-} True else
    let possible = {-(trace $ show (r,c)) $ -}(r, c) `s` dir in
            case (lookup possible m) of
                Just ch => {-(trace "obs") $ -}if ch == '#' then {-(trace $ "turning right at" ++ show (r,c)) $-} isLoop False ((r, c), (turnRight dir)) m
                    else {-(trace $ "forward at" ++ show (r,c) ++ " " ++ show dir) $ -}{-(trace "mov") $ -}isLoop False (possible, dir) (insert (r,c) (directionOf' dir) m)
                Nothing => {-(trace "false")-} False

allObstacleMaps : SortedMap (Int, Int) Char -> List (SortedMap (Int, Int) Char)
allObstacleMaps m = let openSquares = filter (\k => ((fromMaybe '*' (lookup k m)) == '.')) (keys m) in
    map (\sq => insert sq '#' m) openSquares

part2 : String -> Int
part2 input = 
    let inputMap = parseString input
        st = findStart inputMap
        allMaps = allObstacleMaps inputMap
        loopMaps = filter (isLoop True st) allMaps in cast (length loopMaps)

public export
solve : Fin 2 -> String -> Int
solve 0 = part1
solve 1 = part2