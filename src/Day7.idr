module Day7

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities

-- Part 1

-- twoDStringToMap : String -> SortedMap (Int, Int) Char

-- 3267: 81 40 27

parseLine : String -> (Int, List Int)
parseLine s = 
    case map unpack (words s) of
        (test :: inputs) => 
            case test of
                (_ :: _) => (cast (pack (init test)), map (cast . pack) inputs)
                _ => (0, [])
        _ => (0, [])

possibleOutcomes : List Int -> List Int
possibleOutcomes (a :: b :: xs) = possibleOutcomes ((a + b) :: xs) ++ possibleOutcomes ((a * b) :: xs)
possibleOutcomes a = a

-- :: is not :
verify : (Int, List Int) -> Int
verify (a, b) = case find (==a) (possibleOutcomes b) of
    Just _ => a
    Nothing => 0

part1 : String -> Int
part1 input = 
    let parsed = map parseLine (lines input)
        nums = map verify parsed in sum nums

-- Part 2

concat : Int -> Int -> Int
concat a b = cast ((show a) ++ (show b))

possibleOutcomes2 : List Int -> List Int
possibleOutcomes2 (a :: b :: xs) = possibleOutcomes2 ((a + b) :: xs) ++ possibleOutcomes2 ((a * b) :: xs) ++ possibleOutcomes2 ((a `concat` b) :: xs)
possibleOutcomes2 a = a

verify2 : (Int, List Int) -> Int
verify2 (a, b) = case find (==a) (possibleOutcomes2 b) of
    Just _ => a
    Nothing => 0

part2 : String -> Int
part2 input = 
    let parsed = map parseLine (lines input)
        nums = map verify2 parsed in sum nums

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2