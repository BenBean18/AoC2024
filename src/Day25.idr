module Day25

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

-- Part 1

count : (Eq a) => List a -> a -> Int
count (x :: xs) el = (if x == el then 1 else 0) + count xs el
count [] _ = 0

pinHeight : List Char -> Int
pinHeight l = count l '#' - 1

pinHeights : List String -> List Int
pinHeights s = 
    let t = transpose (map unpack s) in map pinHeight t

test : List String
test = lines """
#####
.####
.####
.####
.#.#.
.#...
.....
""" -- 0,5,3,4,3

test2 : List String
test2 = lines """
.....
#....
#....
#...#
#.#.#
#.###
#####
"""

valid : List String -> List String -> Int
valid a b = if (all (<6) (zipWith (+) (pinHeights a) (pinHeights b))) then 1 else 0

isLock : List String -> Bool
isLock ("#####"::xs) = True
isLock _ = False

partial part1 : String -> Int
part1 input =
    let a = forget (splitOn "" (lines input))
        locks = filter isLock a
        keys = filter (not . isLock) a
        valid = sum (map (\lock => sum (map (valid lock) keys)) locks) in valid

-- Part 2

partial part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2