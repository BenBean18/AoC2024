module Day9

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities

-- Part 1

example : String
example = "2333133121414131402"

{-
00...111...2...333.44.5555.6666.777.888899
009..111...2...333.44.5555.6666.777.88889.
0099.111...2...333.44.5555.6666.777.8888..
00998111...2...333.44.5555.6666.777.888...
009981118..2...333.44.5555.6666.777.88....
0099811188.2...333.44.5555.6666.777.8.....
009981118882...333.44.5555.6666.777.......
0099811188827..333.44.5555.6666.77........
00998111888277.333.44.5555.6666.7.........
009981118882777333.44.5555.6666...........
009981118882777333644.5555.666............
00998111888277733364465555.66.............
0099811188827773336446555566..............
 -}

-- input is 94524 characters long

data DiskEntry = Block Int | Empty

Show DiskEntry where
    show (Block a) = show a
    show Empty = "."

Disk : Type
Disk = List DiskEntry

parseDisk' : List Char -> Int -> Disk
parseDisk' (fileSize :: emptySize :: xs) i = (replicate (cast fileSize `minus` (cast '0')) (Block i)) ++ (replicate (cast emptySize `minus` (cast '0')) Empty) ++ parseDisk' xs (i+1)
parseDisk' (fileSize :: []) i = (replicate (cast fileSize `minus` (cast '0')) (Block i))
parseDisk' [] _ = []

parseDisk : List Char -> Disk
parseDisk = (flip parseDisk') 0

isBlock : DiskEntry -> Bool
isBlock (Block _) = True
isBlock Empty = False

isCompacted : (d: Disk) -> Bool
isCompacted (Empty :: xs) = if length (filter isBlock xs) > 0 then False else True
isCompacted (Block _ :: xs) = isCompacted xs
isCompacted [] = True

fillSpace : (d: Disk) -> {auto _ : NonEmpty d} -> Disk
fillSpace (Empty :: xs) = if isCompacted (Empty :: xs) then (Empty :: xs) else
    case xs of
        (_ :: _) => fillSpace ([last xs] ++ (init xs))
        _ => []
fillSpace (Block a :: xs) = if isCompacted (Block a :: xs) then (Block a :: xs) else
    case xs of 
        (_ :: _) => (Block a) :: fillSpace xs
        _ => []

checksum : Disk -> Int
checksum dsk = sum (map (\(a,b) => case a of 
    Block i => i * b
    Empty => 0) (zip dsk [(the Int 0)..(cast (natToInteger (length dsk `minus` 1)))]))

-- show (fillSpace (parseDisk (unpack example)))

part1 : String -> Int
part1 input = let parsed = (parseDisk (unpack input)) in
    case parsed of
        (_ :: _) => checksum (fillSpace parsed)
        _ => 0

-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2