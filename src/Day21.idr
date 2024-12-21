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
decreaseKey : Eq a => Ord a => Show a => BinaryHeap (Int,a,List a) -> (Int,a,List a) -> BinaryHeap (Int,a,List a)
decreaseKey heap (prio,val,prev) = 
    case findIndex (\(_,v,_) => v == val) heap of
        (Just idx') =>
            let idx : Fin (length heap) = idx'
                (currentPrio,currentVal,currentPrev) = index' heap idx
                h : BinaryHeap (Int,a,List a) = (replaceWhen (\(_,v,p) => v == val) (min currentPrio prio, val, if prio < currentPrio then prev else if prio == currentPrio then prev ++ currentPrev else currentPrev) heap)
                upped = (heapifyUp h (finToNat idx) {p2=believe_me (InBounds (finToNat idx) h)}) in 
                upped
        _ => heap

-- hmm so the original version of this function was right lol (it's right here)
backtrack : Show a => SortedMap a (List a) -> a -> List (List a)
backtrack previous current =
    case (lookup current previous) of
        Nothing => [[current]]
        Just [] => [[current]]
        Just previousVertices => concatMap (\p => 
            map (\possiblePath => current :: possiblePath) -- add the current vertex onto those paths
                (backtrack previous p) -- these are all of the paths to get to that vertex
                    ) previousVertices

-- there are few enough shortest paths that this should really be computed in advance and stored in a map
-- we'll see how slow it is though before trying to do that
-- need to reverse moves at end
partial dijkstra : ((Int, Int) -> List (Int, Int)) -> BinaryHeap (Int,(Int,Int),List (Int,Int)) -> SortedMap (Int, Int) (List (Int, Int)) -> (Int, Int) -> SortedMap (Int, Int) (List (Int, Int))
dijkstra _ [] _ _ = empty
dijkstra neighbors unvisited predecessorMap end =
    let (Just (distance,next,prev)) = findMin unvisited in --(trace $ show (next,distance)) $
            if next == end then Data.SortedMap.insert next prev predecessorMap
            else
                let neighs : List (Int, (Int, Int), List (Int, Int)) = map (\n=>(distance+1,n,[next])) (neighbors next)
                    newUnvisited = foldl decreaseKey (deleteMin unvisited) neighs
                    newPredecessorMap = insert next prev predecessorMap
                in 
                --(trace $ show (map fst newUnvisited) ++ "\n\n\n") $ 
                -- 0
                dijkstra neighbors newUnvisited newPredecessorMap end

neighbors' : (Int, Int) -> List (Int, Int)
neighbors' j = map (Utilities.(+) j) [(0,1),(0,-1),(1,0),(-1,0)]

partial shortestPresses : List Char -> (Char -> (Int, Int), (Int, Int) -> Maybe Char, List (Int, Int)) -> (Int, Int) -> List (List Char)
shortestPresses [] _ _ = [[]]
shortestPresses (nextPress::presses) (charToCoord, coordToChar, allCoords) currentPos =
    let nextPos = charToCoord nextPress
        neighbors : (Int, Int) -> List (Int, Int) = (.) (filter (isJust . coordToChar)) neighbors' -- woooooo point free that was cool
        otherVisitable = filter (/= currentPos) allCoords
        unvisited = (0,currentPos,[]) :: (map (\p=>(1000,p,[])) otherVisitable)
        m = dijkstra neighbors unvisited empty nextPos
        posesForAllPaths = backtrack m nextPos
        movesForAllPaths = map (\poses => zipWith (-) ((ne init) poses) ((ne tail) poses)) posesForAllPaths
        charMovesForAllPaths = map (\moves => map directionOf' moves) movesForAllPaths in 
            -- (trace $ "\nMap is " ++ show m ++ 
            -- "\nFrom " ++ show (coordToChar currentPos) ++ "/" ++ show currentPos ++ " to " ++ pack [nextPress] ++ "/" ++ show nextPos ++ 
            -- ": moves=" ++ show movesForAllPaths ++ 
            -- "\nposes=" ++ show posesForAllPaths ++
            -- "\nneighs for ") $ 
            concatMap (\charMoves => map (reverse charMoves ++ ['A'] ++) (shortestPresses presses (charToCoord, coordToChar, allCoords) nextPos)) charMovesForAllPaths

partial first : String -> List (List Char)
first s = shortestPresses (unpack s) (numericKeypad, numericKeypad', numericKeypadCoords) (3,2)

partial second : String -> List (List Char)
second s = 
    let sorted = sortBy (compare `on` length) $ concatMap (\t => shortestPresses t (directionalKeypad, directionalKeypad', directionalKeypadCoords) (0,2)) (first s)
        minLength = length ((ne head) sorted) in filter ((.) ((==) minLength) length) sorted

partial third : String -> List (List Char)
third s =
    let sorted = sortBy (compare `on` length) $ concatMap (\t => shortestPresses t (directionalKeypad, directionalKeypad', directionalKeypadCoords) (0,2)) (second s)
        minLength = length ((ne head) sorted) in filter ((.) ((==) minLength) length) sorted

partial test : IO ()
test = printLn $ map pack (first "179A")
-- ["<^<A^^A>>AvvvA", "<<^A^^A>>AvvvA"]
-- the second one is invalid, left-left-up results on going on an empty square which isn't allowed. what happened?
-- i think the moves are backwards
{-
Map is fromList [((0, 2), [(1, 2)]), ((1, 1), [(2, 1), (1, 2)]), ((1, 2), [(2, 2)]), ((2, 0), [(2, 1)]), ((2, 1), [(3, 1), (2, 2)]), ((2, 2), [(3, 2)]), ((3, 1), [(3, 2)]), ((3, 2), [])]
From Just 'A'/(3, 2) to 1/(2, 0): moves=[[(0, -1), (-1, 0), (0, -1)], [(0, -1), (0, -1), (-1, 0)]]
poses=[[(2, 0), (2, 1), (3, 1), (3, 2)], [(2, 0), (2, 1), (2, 2), (3, 2)]]

so in the second one ^<< works but <<^ doesn't

yeah and we're building the moves list by adding on the current one to the start/cons in backtrack
better to reverse once done i think, since adding at beginning is O(1) but end is O(n) I think?
 -}

partial complexityScore : String -> Int
complexityScore s = cast (length ((ne head) (third s)))

numericPart : String -> Int
numericPart s = cast (pack (filter isDigit (unpack s)))

-- I think what's happening is because there are multiple shortest paths, we have to try them all (one could be more efficient to push at a higher level)

partial part1 : String -> Int
part1 input =
    let l = lines input
        complexities = map complexityScore l
        numbers = map numericPart l in (trace $ show complexities ++ " " ++ show numbers) $ sum (zipWith (*) complexities numbers)

-- yayyyy it worked...in ~30 seconds

-- Part 2

-- WHAT A CHAIN OF 25

-- THAT'S MASSIVE

-- uhhhhhhh

-- i mean the only thing that matters is the total path length
-- but we do need to know intermediates to know where to press
-- i was thinking maybe we just store the length from point to point in a map and add them or something
-- **we can break the problem down more by finding the shortest path for each transition
-- how much faster? benchmarking time!

-- partial part2 : String -> Int
-- part2 input = 
    -- (complexityScore "96") + (complexityScore "65") + (complexityScore "5A") -- 71404us
    -- complexityScore "965A" -- 4262927us (!!!!!!)

-- Ok so that's definitely worth it
-- Wow
-- We need to find a way to save the starting/ending point, but wow
-- Doing that at every level should help a LOT, maximum path length will be like 4 or 5 and we can add them
-- aha and all the subsequent ones end on A, so the starting point IS constant between each level, doesn't matter what path we choose
-- bingo!
-- now time to implement it
-- if we need more speed can precompute all shortest paths

partial onlyMinLength : List (List a) -> List (List a)
onlyMinLength l = 
    let sorted = sortBy (compare `on` length) l
        minLength = length ((ne head) sorted) in filter ((.) ((==) minLength) length) sorted

partial numeric : String -> List (List (List Char))
numeric s = 
    let u = unpack s -- starts on A
        pairs = zip (init ('A'::u)) (tail ('A'::u)) -- i like this idiom or whatever it is lol
        in
        map (\(start,end) => shortestPresses [end] (numericKeypad, numericKeypad', numericKeypadCoords) (numericKeypad start)) pairs

-- returns a list, containing a list of all possible ways (which themselves are char lists) to complete every transition
-- these possibilities for every transition are now the only paths we need to pass to the next iteration

-- add A at the very start, but only at the very start
partial directional : List Char -> List (List (List Char))
directional u = 
    let pairs = zip ((ne init) u) ((ne tail) u) -- i like this idiom or whatever it is lol
        in
        map (\(start,end) => shortestPresses [end] (directionalKeypad, directionalKeypad', directionalKeypadCoords) (directionalKeypad start)) pairs

a : List (List Char)
a = [['<', '^', '<', 'A'], ['^', '<', '<', 'A']] -- poof!

{-
:exec printLn (map directional a)

python for printing:
>>> a = [[[['>', '^', 'A']], [['v', '<', 'A']], [['>', '>', '^', 'A'], ['>', '^', '>', 'A']]], [[['v', '<', 'A']], [['A']], [['>', '>', '^', 'A'], ['>', '^', '>', 'A']]]]
>>> for i in a: print(i)
... 
[[['>', '^', 'A']], [['v', '<', 'A']], [['>', '>', '^', 'A'], ['>', '^', '>', 'A']]] <-- this should be eliminated! it's longer
[[['v', '<', 'A']], [['A']], [['>', '>', '^', 'A'], ['>', '^', '>', 'A']]]
>>> 
 -}

partial nextIterations : List (List Char) -> List (List (List (List Char))) -- listception
nextIterations u = 
    let d : List (List (List (List Char))) = map directional u -- returns a list, of lists for every path, containing a list of all possible paths (which themselves are char lists) for every transition
        lengths = map ((.) sum (map (length . (ne head)))) d
        minLength = (ne head) $ sort lengths
        z = zip d lengths in map fst $ filter (\(_,l) => l == minLength) z

{-
for example (only considering directional rn):
let's say we want to push A<A and figure out the total cost of that.
Day21> :exec printLn (directional (unpack "A<A"))
[                                   this list has two elements, one for each transition

    [                               these are the possible next-level button sequences to do the transition 'A' -> '<' (min/only length: 4)
        ['v', '<', '<', 'A'], 
        ['<', 'v', '<', 'A']
    ], 
    [                               these are the possible next-level button sequences to do the transition '<' -> 'A' (min/only length: 4)
        ['>', '>', '^', 'A'],
        ['>', '^', '>', 'A']
    ]

]
nice formatting is important here. 3D (or 4D *gasp*) lists are hard to think about in a flat form.

we break this problem up into the transitions.

now, we want to check the next level for all four of these until we get to 25.

how do we do that?

well, let's consider the first transition to start.

Day21> :exec printLn (directional ['v', '<', '<', 'A'])
[
    [                               these are the possible next-level button sequences to do the transition 'v' -> '<' (min/only length: 2)
        ['<', 'A']
    ],
    [                               these are the possible next-level button sequences to do the transition '<' -> '<' (min/only length: 1)
        ['A']
    ],
    [                               these are the possible next-level button sequences to do the transition '<' -> 'A' (min/only length: 4)
        ['>', '>', '^', 'A'],
        ['>', '^', '>', 'A']
    ]
]

we really only care about the length. also, this is basically a tree. but, we haven't even finished considering the other option of this transition, so:
Day21> :exec printLn (directional ['<', 'v', '<', 'A'])
[
    [                               these are the possible next-level button sequences to do the transition '<' -> 'v' (min/only length: 2)
        ['>', 'A']
    ],
    [                               these are the possible next-level button sequences to do the transition 'v' -> '<' (min/only length: 2****)
        ['<', 'A']
    ],
    [                               these are the possible next-level button sequences to do the transition '<' -> 'A' (min/only length: 4)
        ['>', '>', '^', 'A'],
        ['>', '^', '>', 'A']
    ]
]

we notice that this second path is less efficient, it takes 2+2+4=8 moves to get there in comparison to 2+1+4=7 moves for the first one.
this is why we have to explore all possibilities, the first way to do the transition was better than the second

so...how do we implement this?

I think this is a BFS, we compare all possibilities at the same level to see which ones are the most efficient.

the list kinda does have to grow each time, i think

final result is the combination of all lengths of all transitions at the last level in the best path
-}

-- We want to filter for min length after seeing all of the possibilities at each level

b : List (List (List Char))
b = [[['>', 'A']], [['<', 'A']], [['>', '>', '^', 'A'], ['>', '^', '>', 'A']]]

partial c : List (List (List Char))
c = numeric "029A"

d : List Char
d = ['<', 'A']

partial finalLength' : {n : Nat} -> List (List (List Char)) -> Int
finalLength' {n=Z} l =
    foldl (\currentSum, thisTransition => currentSum + cast (length ((ne head) thisTransition))) 0 l
finalLength' {n=(S k)} l = sum $ map (\possibleTransitions => -- List (List Char)
    let outcomes : List Int = map (\nextStep => finalLength' {n=k} (directional ('A'::nextStep))) possibleTransitions -- we want the minimum cost for the next step
        in (ne head) (sort outcomes)) l

{-
Day21> :exec printLn (finalLength' {n=0} c)
[
    [
        ['<', 'A']
    ], [
        ['^', 'A']
    ], [
        ['>', '^', '^', 'A'],
        ['^', '>', '^', 'A'],
        ['^', '^', '>', 'A']
    ], 
    [
        ['v', 'v', 'v', 'A']
    ]
]
12

this is correct for one iteration, yay!
multiple is currently broken though
 -}

{-
Day21> :exec printLn (finalLength' {n=1} c)
[[['<', 'A']], [['^', 'A']], [['>', '^', '^', 'A'], ['^', '>', '^', 'A'], ['^', '^', '>', 'A']], [['v', 'v', 'v', 'A']]]
28
Day21> :exec printLn (finalLength' {n=2} c)
[[['v', '<', '<', 'A'], ['<', 'v', '<', 'A']], [['>', '>', '^', 'A'], ['>', '^', '>', 'A']]]
[[['<', 'A']], [['>', 'A']]]
[[['v', 'A']], [['<', '^', 'A'], ['^', '<', 'A']], [['A']], [['>', 'A']]]
[[['<', 'A']], [['v', '>', 'A'], ['>', 'v', 'A']], [['<', '^', 'A'], ['^', '<', 'A']], [['>', 'A']]]
[[['<', 'A']], [['A']], [['v', '>', 'A'], ['>', 'v', 'A']], [['^', 'A']]]
[[['v', '<', 'A'], ['<', 'v', 'A']], [['A']], [['A']], [['>', '^', 'A'], ['^', '>', 'A']]]
[[['<', 'A']], [['^', 'A']], [['>', '^', '^', 'A'], ['^', '>', '^', 'A'], ['^', '^', '>', 'A']], [['v', 'v', 'v', 'A']]]
68
 -}

partial part2 : String -> Int
part2 input = 
    let l = lines input
        complexities = map (\line => finalLength' {n=24} (numeric line)) l
        numbers = map numericPart l in sum (zipWith (*) complexities numbers)

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2