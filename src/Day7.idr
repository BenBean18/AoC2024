module Day7

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities

-- Part 1: 6454us

parseLine : String -> (Int, List Int)
parseLine s = 
    case map unpack (words s) of
        (test :: inputs) => (cast (pack ((ne init) test)), map (cast . pack) inputs)
        _ => (0, [])

verify' : List (Int -> Int -> Int) -> (Int, List Int) -> Bool
verify' fs (test, (a :: b :: xs)) = 
    if a > test then False
    else if b > test then False
    else foldl (\acc, f => 
        acc || verify' fs (test,(f a b :: xs))) False fs
verify' _ (t, [a]) = if t == a then True else False
verify' _ (_, []) = False

verify : List (Int -> Int -> Int) -> (Int, List Int) -> Int
verify fs (a,b) = if verify' fs (a,b) then a else 0

part1 : String -> Int
part1 input = 
    let parsed = map parseLine (lines input)
        nums = map (verify [(*),(+)]) parsed in sum nums

-- order matters for operators I think? if we check * before + it will more likely end early I think
-- ~6600us with * before +, ~6700us with + before *

-- Part 2: 1106876us = 1.107 seconds

-- fun fact: this is O(e^n), where n is the number of operations checked, assuming all operations take the same amount of time
-- https://www.desmos.com/calculator/wc5pp0bxib

concat : Int -> Int -> Int
concat a b = cast ((show a) ++ (show b))

part2 : String -> Int
part2 input =
    let parsed = map parseLine (lines input)
        nums = map (verify [(*),(+),concat]) parsed in sum nums
        -- 1.12 seconds with * before +, 1.24 seconds with + before *
        -- we don't want to put concat first since it takes forever and should be a last resort

-- Exploring parallelization for the first few levels of the tree would be cool
-- GPU? HVM? idk but speedup will be very good

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2