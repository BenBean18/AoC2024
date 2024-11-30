module Day5_23

import Data.String
import Data.List
import Data.List1
import Debug.Trace
import Data.Fin

-- "seeds: 79 14 55 13"
-- [79, 14, 55, 13]
-- HOLY CRAP DEPENDENT TYPING IS ABSOLUTELY INSANE
-- IT LIKE
-- CATCHES INVALID PARSING AS TYPE ERRORS
{-
Day5_23> parseSeeds ""
Error: Can't find an implementation for NonEmpty [].

(Interactive):1:1--1:14
 1 | parseSeeds ""
     ^^^^^^^^^^^^^

Day5_23> parseSeeds "1 2 3"
[2, 3]
-}
parseSeeds : (line:String) -> {auto _ : NonEmpty (words line)} -> List Int
parseSeeds line = map cast (tail (words line))

-- y'know this idea of composing a ton of identical wrapper functions together seems somewhat like monads

data Mapped a = Changed a | Unchanged

parseRangeList : List String -> (Int -> Mapped Int)
parseRangeList (dst :: src :: num :: []) =
    \input => 
        if input >= (cast src) && input < (cast src + cast num) then Changed (input + (cast dst - cast src)) else Unchanged
parseRangeList _ = const Unchanged

parseMapLine : String -> (Int -> Mapped Int)
parseMapLine = parseRangeList . words

combineMappingsSequentially : List (Int -> Int) -> (Int -> Int)
combineMappingsSequentially (f :: fs) = \input => combineMappingsSequentially fs (f input)
combineMappingsSequentially [] = id

combineMappingsParallel : List (Int -> Mapped Int) -> (Int -> Int)
combineMappingsParallel (f :: fs) =
    \input =>
        let output = f input in
        case output of
            Unchanged => combineMappingsParallel fs input
            Changed value => value
combineMappingsParallel [] = id

parseMap : List String -> (Int -> Int)
parseMap lines = combineMappingsParallel (map parseMapLine lines)

public export
part1 : String -> Int
part1 input =
    -- why three colons for a List1/nonempty list, that's weird
    let (seedStrings ::: maps) = splitOn "" (lines input)
        mapper = combineMappingsSequentially (map parseMap maps) in
        case seedStrings of 
            (_ :: []) => let seeds : List Int = map cast (words (head seedStrings)) in
                case seeds of
                    (_ :: actualSeeds) => case actualSeeds of 
                        (_ :: _) => minimum (map mapper actualSeeds) -- this proves that it's non empty!! that's very cool :)
                        _ => 0
                    _ => 0
            _ => 0

-- Part 2

newSeedParser_ : (list : List Int) ->{- {auto p : cast (length list) `mod` 2 = 0} -> -}List Int
newSeedParser_ (start :: num :: xs) = [start..(start+num-1)] ++ newSeedParser_ xs-- {p = listLengthProof xs}
newSeedParser_ [] = []
newSeedParser_ [_] = [] -- this should not happen but idk how to use the type system to force the length to be even yet

newSeedParser : (seeds : List String) -> {auto _ : NonEmpty seeds} -> List Int
newSeedParser seeds = newSeedParser_ (map cast seeds)

public export
part2 : String -> Int
part2 input =
    -- why three colons for a List1/nonempty list, that's weird
    let (seedStrings ::: maps) = splitOn "" (lines input)
        mapper = combineMappingsSequentially (map parseMap maps) in
        case seedStrings of 
            (_ :: []) => let seeds : List String = words $ head seedStrings in
                case seeds of
                    (_ :: actualSeeds) => case actualSeeds of
                        (_ :: _) => let mappedSeeds = map mapper (newSeedParser actualSeeds) in
                            case mappedSeeds of
                                (_ :: _) => minimum mappedSeeds
                                _ => 0
                        _ => 0
                    _ => 0
            _ => 0

public export
solve : Fin 2 -> String -> Int
solve 0 = part1
solve 1 = part2