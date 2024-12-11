module Day11

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.SortedMap

listOfNextStones : Int -> List Int
listOfNextStones 0 = [1]
listOfNextStones s = let str = unpack (show s) in
    if length str `mod` 2 == 0 then
        let (a,b) = splitAt (length str `div` 2) str in [cast (pack a), cast (pack b)]
    else [s * 2024]

-- flip singleton is because we want to be able to give it a value (# occurrences) then a key (stone number) from `map`
mapOfNextStones : Int -> Int -> SortedMap Int Int
mapOfNextStones stone occurrencesOfThatStone = foldl (mergeWith (+)) empty (map ((flip singleton) occurrencesOfThatStone) (listOfNextStones stone))

oneIterationListToMap : SortedMap Int Int -> SortedMap Int Int
oneIterationListToMap m = foldl (mergeWith (+)) empty (map (uncurry mapOfNextStones) (Data.SortedMap.toList m))

numStonesAfter : Int -> String -> Int
numStonesAfter n input =
    let stones = words input
        startingMap = (fromList (map (,1) (map cast stones)))
        iteratedStones = foldl (\m, _ => oneIterationListToMap m) startingMap [1..n] in sum (values iteratedStones)

part1 : String -> Int -- 2.543ms
part1 input = numStonesAfter 25 input

part2 : String -> Int -- 101.591ms
part2 input = numStonesAfter 75 input

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2