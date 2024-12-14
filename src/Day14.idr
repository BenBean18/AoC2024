module Day14

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

-- late start b/c minifrc

{-
p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
 -}

-- +x = right, +y = down (normal yay)

{-
These robots have a unique feature for maximum bathroom security: they can teleport.
When a robot would run into an edge of the space they're in, they instead teleport to the other side, effectively wrapping around the edges.

--> modular arithmetic, probably
 -}

-- Where will the robots be after 100 seconds?
-- 11 tiles wide, 7 tiles tall for example
-- (full space is 101 wide and 103 tall)

data Robot = MkRobot (Int, Int) (Int, Int)

partial parseRobot : String -> Robot
parseRobot s = case (words s) of
    (ps :: vs :: []) => 
        let posSplit : List (List Char) = map (filter (\x => isDigit x || x == '-')) (forget (splitOn ',' (unpack ps))) in
            case (map pack posSplit) of
                (px :: py :: []) =>
                    let velSplit : List (List Char) = map (filter (\x => isDigit x || x == '-')) (forget (splitOn ',' (unpack vs))) in
                        case (map pack velSplit) of
                            (vx :: vy :: []) => MkRobot (cast px, cast py) (cast vx, cast vy)
                            -- _ => MkRobot (0,0) (0,0)
                -- _ => MkRobot (0,0) (0,0)
    -- _ => MkRobot (0,0) (0,0)

robotAfter100 : Robot -> (Int, Int)
robotAfter100 (MkRobot (px,py) (vx,vy)) = ((px + (vx * 100)) `mod` 101, (py + (vy * 100)) `mod` 103)

isWithin : ((Int, Int),(Int, Int)) -> (Int, Int) -> Int
isWithin ((lx,ly),(ux,uy)) (x,y) = if lx <= x && x <= ux && ly <= y && y <= uy then 1 else 0

quadrants : List ((Int, Int), (Int, Int))
-- quadrants = [((0,0),(4,2)),((6,0),(10,2)),((0,4),(4,6)),((6,4),(10,6))]
quadrants = [((0,0),((100 `div` 2 - 1),(102 `div` 2 - 1))) -- tl
            ,(((100 `div` 2 + 1),0),(100,(102 `div` 2 - 1))) -- tr
            ,((0,(102 `div` 2 + 1)),((100 `div` 2 - 1),102)), -- bl
            (((100 `div` 2 + 1),(102 `div` 2 + 1)),(100,102))] -- br

addLists : List Int -> List Int -> List Int
addLists a b = zipWith (+) a b

partial part1 : String -> Int
part1 input =
    let robots = map parseRobot (lines input)
        finalPositions = map robotAfter100 robots
        quadrantMaps = foldl addLists [0,0,0,0] (map (\pos => map ((flip isWithin) pos) quadrants) finalPositions) in foldl (*) 1 quadrantMaps

-- Part 2

part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2