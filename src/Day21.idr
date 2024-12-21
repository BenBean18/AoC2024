module Day21

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
import MinHeap

-- Part 1

-- (y,x)
partial numericKeypad : Char -> (Int, Int)
numericKeypad '7' = (0,0)
numericKeypad '8' = (0,1)
numericKeypad '9' = (0,2)
numericKeypad '4' = (1,0)
numericKeypad '5' = (1,1)
numericKeypad '6' = (1,2)
numericKeypad '1' = (2,0)
numericKeypad '2' = (2,1)
numericKeypad '3' = (2,2)
-- numericKeypad ' ' = (3,0)
numericKeypad '0' = (3,1)
numericKeypad 'A' = (3,2)

-- (y,x)
numericKeypad' : (Int, Int) -> Maybe Char
numericKeypad' (0,0) = Just '7' 
numericKeypad' (0,1) = Just '8' 
numericKeypad' (0,2) = Just '9' 
numericKeypad' (1,0) = Just '4' 
numericKeypad' (1,1) = Just '5' 
numericKeypad' (1,2) = Just '6' 
numericKeypad' (2,0) = Just '1' 
numericKeypad' (2,1) = Just '2' 
numericKeypad' (2,2) = Just '3' 
-- numericKeypad (3,0) = ' '
numericKeypad' (3,1) = Just '0' 
numericKeypad' (3,2) = Just 'A' 
numericKeypad' _ = Nothing

numericKeypadCoords : List (Int, Int)
numericKeypadCoords = (0,0) :: (0,1) :: (0,2) :: (1,0) :: (1,1) :: (1,2) :: (2,0) :: (2,1) :: (2,2) :: (3,1) :: (3,2) :: []

directionOf' : (Int, Int) -> Char
directionOf' (0,1) = '>' 
directionOf' (0,-1) = '<'
directionOf' (1,0) = 'v'
directionOf' (-1,0) = '^'
directionOf' _ = 'X'

-- (y,x)
partial directionalKeypad : Char -> (Int, Int)
-- directionalKeypad ' ' = (0,0)
directionalKeypad '^' = (0,1)
directionalKeypad 'A' = (0,2)
directionalKeypad '<' = (1,0)
directionalKeypad 'v' = (1,1)
directionalKeypad '>' = (1,2)

-- (y,x)
directionalKeypad' : (Int, Int) -> Maybe Char
-- directionalKeypad' (0,0) = ' '
directionalKeypad' (0,1) = Just '^'
directionalKeypad' (0,2) = Just 'A'
directionalKeypad' (1,0) = Just '<'
directionalKeypad' (1,1) = Just 'v'
directionalKeypad' (1,2) = Just '>'
directionalKeypad' _ = Nothing

directionalKeypadCoords : List (Int, Int)
directionalKeypadCoords = (0,1) :: (0,2) :: (1,0) :: (1,1) :: (1,2) :: []

-- For pathfinding, a space is invalid

-- Control flow:
-- Human directly types on directional keypad #4, which controls
-- Robot3, which types on directional keypad #3, which controls
-- Robot2, which types on directional keypad #2, which controls
-- Robot1, which types on the numeric keypad #1, which opens the door

-- We only care about the length of the shortest path, which might help?

-- This is a ginormous search space
-- I think the minimum number of button presses at each stage is always good
-- But... it's better to press two adjacent buttons on the higher-level keypad than have to move all the way across to achieve the same outcome
-- Maybe Dijkstra where the cost function takes into account the distance between buttons at the higher level?
-- I'm not sure if it matters though, the example just uses the shortest path at every level

-- Need to press A after getting to each position
-- We need to find the shortest path between pairs of coordinates
-- hello Edsger, I think I'll be using your algorithm again
decreaseKey : Eq a => Ord a => Show a => BinaryHeap (Int,a,a) -> (Int,a,a) -> BinaryHeap (Int,a,a)
decreaseKey heap (prio,val,prev) = 
    case findIndex (\(_,v,_) => v == val) heap of
        (Just idx') =>
            let idx : Fin (length heap) = idx'
                (currentPrio,currentVal,currentPrev) = index' heap idx
                h : BinaryHeap (Int,a,a) = (replaceWhen (\(_,v,p) => v == val) (min currentPrio prio, val, if prio < currentPrio then prev else currentPrev) heap)
                upped = (heapifyUp h (finToNat idx) {p2=believe_me (InBounds (finToNat idx) h)}) in 
                upped
        _ => heap

backtrack : SortedMap (Int,Int) (Int,Int) -> (Int,Int) -> List (Int,Int)
backtrack previous current = --(trace $ show current) $
    case lookup current previous of
        Nothing => [current]
        Just (-1,-1) => [current]
        Just vertex => current :: backtrack previous vertex

-- there are few enough shortest paths that this should really be computed in advance and stored in a map
-- we'll see how slow it is though before trying to do that
-- need to reverse moves at end
partial dijkstra : ((Int, Int) -> List (Int, Int)) -> BinaryHeap (Int,(Int,Int),(Int,Int)) -> SortedMap (Int, Int) (Int, Int) -> (Int, Int) -> SortedMap (Int, Int) (Int, Int)
dijkstra _ [] _ _ = empty
dijkstra neighbors unvisited predecessorMap end =
    let (Just (distance,next,prev)) = findMin unvisited in --(trace $ show (next,distance)) $
            if next == end then Data.SortedMap.insert next prev predecessorMap
            else
                let neighs : List (Int, (Int, Int), (Int, Int)) = map (\n=>(distance+1,n,next)) (neighbors next)
                    newUnvisited = foldl decreaseKey (deleteMin unvisited) neighs
                    newPredecessorMap = insert next prev predecessorMap

                in 
                --(trace $ show (map fst newUnvisited) ++ "\n\n\n") $ 
                -- 0
                dijkstra neighbors newUnvisited newPredecessorMap end

neighbors' : (Int, Int) -> List (Int, Int)
neighbors' j = map (Utilities.(+) j) [(0,1),(0,-1),(1,0),(-1,0)]

partial shortestPresses : List Char -> (Char -> (Int, Int), (Int, Int) -> Maybe Char, List (Int, Int)) -> (Int, Int) -> List Char
shortestPresses [] _ _ = []
shortestPresses (nextPress::presses) (charToCoord, coordToChar, allCoords) currentPos =
    let nextPos = charToCoord nextPress
        neighbors : (Int, Int) -> List (Int, Int) = (.) (filter (isJust . coordToChar)) neighbors' -- woooooo point free that was cool
        otherVisitable = filter (/= currentPos) allCoords
        unvisited = (0,currentPos,(-1,-1)) :: (map (\p=>(1000,p,(-1,-1))) otherVisitable)
        m = dijkstra neighbors unvisited empty nextPos
        poses = backtrack m nextPos
        moves = zipWith (-) ((ne init) poses) ((ne tail) poses)
        charMoves = map directionOf' moves in
            (trace $ "\nMap is " ++ show m ++ 
            "\nFrom " ++ show (coordToChar currentPos) ++ "/" ++ show currentPos ++ " to " ++ pack [nextPress] ++ "/" ++ show nextPos ++ 
            ": moves=" ++ show moves ++ 
            "\nposes=" ++ show poses) $ 
            charMoves ++ ['A'] ++ shortestPresses presses (charToCoord, coordToChar, allCoords) nextPos

partial test : List Char
test = shortestPresses (unpack "029A") (numericKeypad, numericKeypad', numericKeypadCoords) (3,2)

partial part1 : String -> Int
part1 input = 1

-- Part 2

partial part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2