module Day11

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.SortedMap

-- Part 1

{-

    If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
    If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
    If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.

The order doesn't matter, we just need to know how many stones there are
The specific number doesn't matter either, unless it's zero

 -}

-- about to get rolled by part 2
nextStone : Int -> List Int
nextStone 0 = [1]
nextStone s = let str = unpack (show s) in if length str `mod` 2 == 0 then
    let (a,b) = splitAt (length str `div` 2) str in [cast (pack a), cast (pack b)] else [s * 2024]

oneIteration : List Int -> List Int
oneIteration l = concatMap nextStone l

part1 : String -> Int
part1 input =
    let stones = words input
        iteratedStones = foldl (\acc, elem => (trace "hi") $ oneIteration acc) (map cast stones) [1..25] in cast $ length iteratedStones

-- weird number theory stuff, what happens when mult by 2024
-- modular arithmetic??

-- starting from 0 is predictable, 0 -> 1 -> 2024 -> 20 24 -> 
-- wait we can just store the number of digits? maybe?

-- we need to be able to store the number of zero stones, the number of even-digit stones, and the number of odd-digit stones

{-
[125] odd
[253000] even
[253, 0] odd, even
[512072, 1] even, odd
[512, 72, 2024] 
[1036288, 7, 2, 20, 24]
[2097446912, 14168, 4048, 2, 0, 2, 4]
[20974, 46912, 28676032, 40, 48, 4048, 1, 4048, 8096]
[42451376, 94949888, 2867, 6032, 4, 0, 4, 8, 40, 48, 2024, 40, 48, 80, 96]
 -}


-- even : Int -> String
-- even a = if a `mod` 2 == 0 then "E" else "O"

-- memoizedNextStone : SortedMap Int (List Int) -> Int -> (SortedMap Int (List Int), List Int)
-- memoizedNextStone memo input =
--     case lookup input memo of
--         Just output => (trace "found") pure output
--         Nothing => 
--             let output = nextStone input in (insert input output memo, output)

-- oneIterationMemoized : SortedMap Int (List Int) -> List Int -> List Int
-- oneIterationMemoized memo l = foldl (\(memo, outputList), el => ) (empty, l)

-- Try storing numbers as an occurrence list, only 296 unique as of 20 iterations

-- map needs to be (key, occurrences) NOT (occurrences, key) :duh:
-- fromList [(1, lastNum)] is not correct

nextStone' : Int -> Int -> SortedMap Int Int
nextStone' multiplier i = foldl (mergeWith (+)) empty (map (\a => fromList [(a,multiplier)]) (nextStone i))

-- why is this called merge not union
oneIterationListToMap : SortedMap Int Int -> SortedMap Int Int
oneIterationListToMap m = foldl (Data.SortedMap.mergeWith (+)) empty (map (\(val,mult) => nextStone' mult val) (Data.SortedMap.toList m))

part2 : String -> Int
part2 input = 
    let stones = words input
        startingMap = (fromList (map (,1) (map cast stones))) -- testing this with 0 0 made me realize why 77 (prev written same way) was broken, keys were overwriting each other
        iteratedStones = foldl (\acc, elem => oneIterationListToMap acc) startingMap [1..75] in
            sum $ values iteratedStones

{-
1 O
1 E
2 OE
2 EO
3 EEE
5 EOEEE
7 EEEEEEE
9 EEEEEEOEE
15 EEOEEEEEEEEEEEE
26 OEEEEOEEEOEEEEEEEEEEEEEEOE
38 EOOEEEEEEEEOEEOEEEEEEEEOEEEEEEEOEEEOEE
56 EEEOOOOEOEOEOEEEEEEEEOEEEEOEEEEEOEOEEEEEEEEOEEEEEEEEEEEE
82 EEEEEEEEEEEEEEEEEEEEEEEEEEEEOEEEEEEEOEEEOEEEEOEEEEEOEOEEEEEEEEEEOEEEEEOEOEOEEEEEOE
 -}

-- odd always splits into two evens
-- if number's digits are less than (10000/2024) 4.940711462450593 then it stays even, otherwise it goes odd

-- so the data does matter for each even, it's not just the number of digits

-- can a polynomial be fit to this

-- oeis?

-- no answer given for sample pt2, maybe it relies on a property of the input?

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2