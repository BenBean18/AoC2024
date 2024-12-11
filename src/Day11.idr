module Day11

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities

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

-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2