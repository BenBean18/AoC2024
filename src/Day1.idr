module Day1

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Data.Vect

-- Part 1

parseLine : String -> (Int, Int)
parseLine line = let nums : List Int = map cast (words line) in
    case nums of
        (a :: b :: []) => (a, b)
        _ => (0, 0)

-- Want to take [[1, 2], [3, 4], [5, 6]] and get [1,3,5], [2,4,6]

toLists : List (Int, Int) -> ((List Int), (List Int))
toLists l = unzip l

difference : (Int, Int) -> Int
difference (a, b) = abs (a - b)

part1 : String -> Int
part1 input = 
    let lines = map parseLine (lines input)
        (a, b) = toLists lines
        (sortedA, sortedB) = (sort a, sort b)
        rezipped = zip sortedA sortedB in sum (map difference rezipped)

-- Part 2

count : (Eq a) => List a -> a -> Int
count (x :: xs) el = (if x == el then 1 else 0) + count xs el
count [] _ = 0

part2 : String -> Int
part2 input =
    let lines = map parseLine (lines input)
        (a, b) = toLists lines
        counts = map (\el => (el * count b el)) a in sum counts

public export
solve : Fin 2 -> String -> Int
solve 0 = part1
solve 1 = part2