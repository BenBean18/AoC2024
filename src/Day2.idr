module Day2

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Data.Nat

-- Part 1

allIncreasingOrDecreasing : (Eq a, Ord a) => List a -> Bool
allIncreasingOrDecreasing l = (l == sort l) || (l == reverse (sort l))

-- ...https://github.com/BenBean18/AoC2023/blob/main/src/Day9.hs#L20-L21
differenceList : List Int -> List Int
differenceList ints = case ints of
    (_ :: _) => map abs (zipWith (-) (tail ints) (init ints))
    _ => []

isSafe : (xs : List Int) -> Int
isSafe l =  let differences = differenceList l
                sortedDiffs = sort differences in
                case sortedDiffs of
                    (_ :: _) =>
                        let minimum = head sortedDiffs
                            maximum = last sortedDiffs
                            incDec = allIncreasingOrDecreasing l in (if (minimum >= 1 && (maximum <= 3) && incDec) then 1 else 0)
                    _ => 0

part1 : String -> Int
part1 input = let theLines : List (List Int) = map (map cast) (map words (lines input)) in
    case theLines of 
        (_ :: _) => sum (map isSafe theLines)
        _ => 0

-- Part 2

removeAt : List a -> Nat -> List a
removeAt l idx = take idx l ++ drop (idx+1) l

allPossibilities : List a -> List (List a)
allPossibilities l = let ps = map (removeAt l) [0..(length l)] in case ps of
    (_ :: _) => init ps
    _ => []

newIsSafe : List Int -> Int
newIsSafe l = if sum (map isSafe (allPossibilities l)) > 0 then 1 else 0

part2 : String -> Int
part2 input = let theLines : List (List Int) = map (map cast) (map words (lines input)) in
    case theLines of 
        (_ :: _) => sum (map newIsSafe theLines)
        _ => 0

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2