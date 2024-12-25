module Day6

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.SortedMap
import System.Concurrency
import Data.IORef

-- Part 1: runs in 11.773ms

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

move : ((Int, Int), (Int, Int)) -> SortedMap (Int, Int) Char -> SortedMap (Int, Int) Char
move ((r, c), dir) m =
    let possible = (r, c) + dir in
            case (lookup possible m) of
                Just ch => if ch == '#' then move ((r, c), (turnRight dir)) m
                    else move (possible, dir) (insert (r,c) 'X' m)
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
    let inputMap = twoDStringToMap input
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
isLoop isFirst ((r, c), dir) m = if directionOf (fromMaybe '.' (lookup (r,c) m)) == dir && not isFirst then True else
    let possible = (r, c) + dir in
            case (lookup possible m) of
                Just ch => if ch == '#' then isLoop False ((r, c), (turnRight dir)) m
                    else isLoop False (possible, dir) (insert (r,c) (directionOf' dir) m)
                Nothing => False

allObstacleMaps : SortedMap (Int, Int) Char -> List (SortedMap (Int, Int) Char)
allObstacleMaps m = let openSquares = filter (\k => ((fromMaybe '*' (lookup k m)) == 'X')) (keys m) in
    map (\sq => insert sq '#' m) openSquares

numPerThread : Nat
numPerThread = 500

part2 : String -> IO Int
part2 input = do
    let inputMap = twoDStringToMap input
        st = findStart inputMap
        result = move st inputMap
        allMaps = allObstacleMaps result
        groupedMaps = grouped numPerThread allMaps
    m <- makeMutex
    ref <- newIORef (the Int 0)
    threadIDs <- traverse (\maps => fork $ do
            putStrLn "Starting thread"
            let val = cast $ length $ filter (isLoop True st) maps
            mutexAcquire m
            modifyIORef ref (\current => current + val)
            mutexRelease m
            ) groupedMaps
    _ <- traverse (\i => threadWait i) threadIDs
    readIORef ref

-- fork splits off a thread

-- An optimization is to only place obstacles along the original path, cuts search space by roughly 4x

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = part2