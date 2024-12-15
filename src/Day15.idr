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
import Data.Vect

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
moveRight ('#'::xs) = '#'::xs -- stop if no space
moveRight (c::'.'::xs) = '.'::c::xs -- swap if open space

-- leave leftmost character (no swap opportunity), move the right section, then try to move the whole thing if new open dot
moveRight (x::xs) = let new = x::moveRight xs in if new /= (x::xs) then moveRight new else new
moveRight [] = []

-- yes, x and y are transposed here, it's ok as long as it's consistent within the function
genRange : (Int, Int) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
genRange (x,y) dir (maxX, maxY) =
    if x > maxX || y > maxY then []
    else if x < 0 || y < 0 then []
    else
        let next = (x,y) + dir in (x,y) :: genRange next dir (maxX, maxY)

moveFromDot : SortedMap (Int, Int) Char -> (Int, Int) -> SortedMap (Int, Int) Char
moveFromDot m (0,0) = m
moveFromDot m dir =
    let l : List ((Int, Int), Char) = toList m
        (ry,rx) : (Int, Int) = fst $ (ne head) (filter (\(k,v) => v == '@') l)
        s = sort (keys m)
        (maxY, maxX) = ne last s
        positions : List (Int, Int) = genRange (ry,rx) dir (maxY, maxX)
        values : List Char = map (\p => fromMaybe '.' (lookup p m)) positions
        newEntries = zip positions (moveRight values) in (trace $ show (ry,rx){- ++ show (pack values) ++ "\n" ++ render2DMap m-}) $ mergeLeft (fromList newEntries) m

score : SortedMap (Int, Int) Char -> Int
score m = 
    let l : List ((Int, Int), Char) = toList m
        boxes = filter (\(k,v) => v == 'O') l
        coords : List (Int, Int) = map fst boxes
        scores = map (\(y, x) => (y*100 + x)) coords in sum scores

partial part1 : String -> Int
part1 input =
    let (m' ::: (instructions' :: [])) = splitOn "" (lines input)
        m : SortedMap (Int, Int) Char = twoDStringToMap (unlines m')
        instructions'' : String = unlines instructions'
        instructions = map directionOf (unpack instructions'')
        t : List (Int, Int) = [(0,-1)]
        finalState = foldl moveFromDot m instructions in (trace $ show instructions ++ "\n" ++ render2DMap finalState) (score finalState)

-- Part 2

-- collision something? there must be a fast way to do this
-- seems kind of like a physics engine, honestly
-- basically you try pushing until you hit a wall
-- maybe mark each cube as immovable in a specific direction if we've failed once
-- but that's hard to update
-- represent each cube as a polygon and like send a ray in the moving direction from the robot or something?
-- once the ray hits an empty space, move everything "selected" by it before that empty space one unit in its direction
-- if the ray hits a wall before then, do nothing
-- this can simply be a filter on the list of polygons/walls, I think?
-- then sort and look for the first empty space or wall
-- this feels horribly inefficient, but much better than doing things with strings and converting maps (cough cough part 1)

-- these will be squares, format: type tl br
data Object = Block (Int, Int) (Int, Int) | Wall (Int, Int) (Int, Int)

part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2