module Day15

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Data.Nat
import Utilities
import Data.SortedMap
import Data.SortedSet
import Data.List1

-- Part 1

-- sokoban?

-- https://github.com/BenBean18/AoC2023/blob/0cfae9c97def9a82df5a849b37c2fc737ece2662/src/Day23.hs#L37C1-L42C22
directionOf : Char -> (Int, Int)
directionOf '>' = (0,1)
directionOf '<' = (0,-1)
directionOf 'v' = (1,0)
directionOf '^' = (-1,0)
directionOf _ = (0,0)

{-
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
 -}

-- OOO# cannot be moved
-- ooh what if we approach
-- moveBoxesRight : List Char -> (Bool, List Char)
-- moveBoxesRight ('#'::xs) = (False, '#'::xs)
-- moveBoxesRight ('.'::xs) = (True, '.'::xs)
-- moveBoxesRight ('O'::xs) = 
-- moveBoxesRight l = (True, l) -- shouldn't happen

-- a move is valid if the list from the robot in the direction of the move to the first '#' has at least one dot
findMovablePart : List Char -> List Char
-- findMovablePart ('.'::xs) = ['.']
findMovablePart ('#'::xs) = []
findMovablePart (c::xs) = c :: findMovablePart xs
findMovablePart [] = []

-- test cases:
-- moveRight (unpack "@O.O.#") --> "@O."
-- findMovablePart (unpack "@O#") --> ""
-- @. --> .@

moveRight : List Char -> List Char
moveRight (c::'.'::xs) = '.'::c::xs -- move if open space
moveRight ('#'::xs) = '#'::xs
moveRight (x::xs) = moveRight (x::moveRight xs)
moveRight [] = []

-- so if this is valid/has empty space, we move the robot right one and cycle the list
-- @O.O.# > .@OO.#
-- again, relevant part is only before the first dot
-- @O. > .@O
-- @OOOO. > .@OOOO

-- tryMoveRight : List Char -> 

-- pushing a chain of boxes
-- tryMove : List (List Char) -> (Int, Int) -> (Int, Int) -> List (List Char)
-- tryMove m pos move =
--     let next = pos + move in
--         if canMove (fromMaybe '.' (lookup m next)) then



part1 : String -> Int
part1 input = 1

-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2