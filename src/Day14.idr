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
import System

-- Part 1: 437us

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

-- Part 2: 15sec...need to optimize but will sleep first

-- If they're the same type of robots, they should have a hard-coded Easter egg: very rarely, most of the robots should arrange themselves into a picture of a Christmas tree.

render2DMap' : SortedMap (Int, Int) Char -> String
render2DMap' m =
    let s = sort (keys m)
        (maxY, maxX) = (103,101)
        (minY, minX) = (0,0)
        toRender = map (\y => pack $ map (\x => (fromMaybe ' ') (lookup (cast x,cast y) m)) [minX..maxX]) [minY..maxY] in unlines toRender

renderMap : List (Int, Int) -> String
renderMap l = render2DMap' (fromList (map (,'#') l))

-- I don't know what the Christmas tree looks like, so this is probably cycle detection, because it says this happens very rarely, implying that it cycles through doing this? maybe?
-- LCM of all of the cycles of each robot?

-- findCycle : Robot -> Int

next : Robot -> Robot
next (MkRobot (px,py) (vx,vy)) = MkRobot ((px + vx) `mod` 101, (py + vy) `mod` 103) (vx,vy)

positionOf : Robot -> (Int, Int)
positionOf (MkRobot p _) = p

renderRobots : List Robot -> Int -> IO ()
renderRobots _ 10404 = pure ()
renderRobots l i = do
    putStr ("\n\n" ++ show i ++ "\n")
    let rendered = renderMap (map positionOf l)
    let border = "###############################"
    let containsBorder = border `isInfixOf` rendered
    putStrLn rendered
    renderRobots (map next l) (i + 1)

findTree : List Robot -> Int -> Int
findTree l i = if "###############################" `isInfixOf` (renderMap (map positionOf l)) then i else findTree (map next l) (i+1)

Eq Robot where
    (==) (MkRobot p1 v1) (MkRobot p2 v2) = p1 == p2 && v1 == v2

-- wait, this is a cycle detection problem definitely
-- if a tree shows itself, it HAS to repeat since this wraps around and is constant position/velocity
findCycle : Robot -> Robot -> Bool -> Int -> Int
findCycle init current isFirst num =
    if init == current && not isFirst then num
    else
        findCycle init (next current) False (num + 1)

-- https://stackoverflow.com/a/7922967
-- Euclidean algorithm
gcd : Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

-- copied from Haskell source
lcm             : Int -> Int -> Int
{-# SPECIALISE lcm :: Int -> Int -> Int #-}
{-# SPECIALISE lcm :: Word -> Word -> Word #-}
{-# NOINLINE [2] lcm #-} -- See Note [Allow time for type-specialisation rules to fire]
lcm _ 0         =  0
lcm 0 _         =  0
lcm x y         =  abs ((x `div` (gcd x y)) * y)

-- https://discourse.haskell.org/t/folding-a-lcm-of-a-list/841
lcmm : List Int -> Int
lcmm [] = 1
lcmm (x::xs) = lcm x (lcmm xs)

-- 10403 is the time for the image to cycle, so we need to examine 0-10403
-- oh wait that makes sense lol
-- it's the size of the image

-- From visually inspecting, it looks like the more Christmas-tree-like ones are less random, i.e. more squares are zero

count : (Eq a) => List a -> a -> Int
count (x :: xs) el = (if x == el then 1 else 0) + count xs el
count [] _ = 0

zeroSquares : SortedMap (Int, Int) Char -> Int
zeroSquares m =
    let s = sort (keys m)
        (maxY, maxX) = (103,101)
        (minY, minX) = (0,0) in sum (map (\y => sum (map (\x => if isJust (lookup (cast x,cast y) m) then 0 else 1) [minX..maxX])) [minY..maxY])

partial part2 : String -> IO Int
part2 input =
    let robots = map parseRobot (lines input)
        -- cycles = map (\r => findCycle r r True 0) robots
        -- fullCycle = lcmm cycles in pure fullCycle
    -- in
    -- do
    --     renderRobots robots 0
    --     pure 2
    in pure (findTree robots 0)

-- the way I solved this is really dumb, I figured a Christmas tree would have lots of robots in a row so searched
-- for more and more #s in my terminal until only one was left (in the 10403 unique maps), and that showed a Christmas
-- tree. See `day14_soln_kinda.png`.

-- I want to do this programmatically though

-- this is such a cool idea! https://www.reddit.com/r/adventofcode/comments/1hdvhvu/comment/m1z79gn/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
-- also awesome https://www.reddit.com/r/adventofcode/comments/1he6in6/2024_day_14_part_2_a_different_approach/

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = part2