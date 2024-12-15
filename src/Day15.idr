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

directionOf' : (Int, Int) -> Char
directionOf' (0,1) = '>' 
directionOf' (0,-1) = '<'
directionOf' (1,0) = 'v'
directionOf' (-1,0) = '^'
directionOf' _ = 'X'

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

flip : (Int, Int) -> (Int, Int)
flip (a,b) = (b,a)

rerender : SortedSet (Int, Int) -> SortedSet (Int, Int) -> SortedMap (Int, Int) Char
rerender blocks walls =
    let blockMap : SortedMap (Int, Int) Char = fromList $ map (,'[') (Data.SortedSet.toList blocks)
        otherSide : List (Int, Int) = (map (\(y,x)=>(y,x+1)) (Data.SortedSet.toList blocks))
        otherBlockMap : SortedMap (Int, Int) Char = fromList $ map (,']') otherSide
        wallMap : SortedMap (Int, Int) Char = fromList $ map (,'#') (Data.SortedSet.toList walls) in mergeLeft blockMap (mergeLeft otherBlockMap wallMap)

-- blocks will be represented as left coordinate, i.e. only the [
-- SortedSet looks to be implemented with a SortedMap which looks to be a tree map, so O(log n)
-- HashMap/HashSet would be better, but hopefully this is fast enough since I don't think Idris has those builtin

compose : List (Bool, (SortedSet (Int, Int) -> SortedSet (Int, Int))) -> (Bool, (SortedSet (Int, Int) -> SortedSet (Int, Int)))
compose ((True, fn)::xs) =
    let (good,f) = compose xs in if not good then (False, id) else (True, fn . f)
compose ((False, _) :: _) = (False, id) -- if we reach an invalid move, end early and just return identity
compose [] = (True, id)

-- weird because side is treated differently than vertical
neighboringBlockLocations : (Int, Int) -> List (Int, Int)
-- down is down, down left, down right
neighboringBlockLocations (1,0) = [(1,0),(1,-1),(1,1)]
-- up is up, up left, up right
neighboringBlockLocations (-1,0) = [(-1,0),(-1,-1),(-1,1)]
-- left is just two to the left, if it's one to the left that's an intersection
neighboringBlockLocations (0,-1) = [(0,-2)]
-- right is two to the right, if it's one to the right that's an intersection
neighboringBlockLocations (0,1) = [(0,2)]
neighboringBlockLocations _ = []

-- weird because side is treated differently than vertical
neighboringPushLocations : (Int, Int) -> List (Int, Int)
-- down is down, down left
neighboringPushLocations (1,0) = [(1,0),(1,-1)]
-- up is up, up left
neighboringPushLocations (-1,0) = [(-1,0),(-1,-1)]
-- left is two to the left, if it's one to the left that's an intersection
neighboringPushLocations (0,-1) = [(0,-2)]
-- right is one to the right
neighboringPushLocations (0,1) = [(0,1)]
neighboringPushLocations _ = []

{-
<
cur (6, 3) blocked
####################
##[]  []      [][]##
##[]           [] ##
##        [][][][]##
##         []   []##
##  ##[]          ##
## [][]@          ##
##     [][]   [][]##
##       []   []  ##
####################

<
####################
##[]  []      [][]##
##[]           [] ##
##        [][][][]##
##         []   []##
##  ##[]          ##
## [][]@          ##
##     [][]   [][]##
##       []   []  ##
####################

this is wrong, the block should not be checking two to the left for a wall since it's already the left side, fixed below
WORKS ON BIG EXAMPLE NOW
 -}

-- weird because side is treated differently than vertical
wallCheckingLocations : (Int, Int) -> List (Int, Int)
-- down is down, down left, down right
wallCheckingLocations (1,0) = [(1,0),(1,1)]
-- up is up, up left, up right
wallCheckingLocations (-1,0) = [(-1,0),(-1,1)]
-- left is just two to the left, if it's one to the left that's an intersection
wallCheckingLocations (0,-1) = [(0,-1)]
-- right is two to the right, if it's one to the right that's an intersection
wallCheckingLocations (0,1) = [(0,2)]
wallCheckingLocations _ = []

{-
^
####################
##    []    []  []##
##            []  ##
##  [][]    []  []##
##   []   []  []  ##
##[]##    []      ##
##[][]        []  ##
## @   []  [] [][]##
##        []      ##
####################

this is incorrect behavior, the stack on the left is movable. why is this happening?
oh it's because one of the neighbors is a wall, that's ok
 -}

tryMove : (Int, Int) -> (Int, Int) -> SortedSet (Int, Int) -> SortedSet (Int, Int) -> (Bool, (SortedSet (Int, Int) -> SortedSet (Int, Int)))
tryMove cur dir blocks walls =
    let neighbors: List (Int, Int) = map (+ cur) (neighboringBlockLocations dir) -- forward, one forward-left, one forward-right
        toCheckForWall : List (Int, Int) = map (+ cur) (wallCheckingLocations dir) -- don't need to check forward-left for wall, that ONLY interferes if it's a block (because we're storing all blocks of walls, not just the left side)
    in
    if any (`contains` walls) toCheckForWall then
        (trace $ "cur " ++ show cur ++ " blocked") $ (False, id) -- blocked by all walls, can't move
    else if (not (any (`contains` walls) toCheckForWall)) && not (any (`contains` blocks) neighbors) then
        (True, (\s => insert (cur + dir) (delete cur s))) -- empty space ahead, free to move
    else
        let blockNeighbors = filter (`contains` blocks) neighbors in
        -- VERY important that we only call this function with blocks, I had just `neighbors` before as the list for the map
        compose $ (True, (\s => insert (cur + dir) (delete cur s))) :: (map (\n => tryMove n dir blocks walls) blockNeighbors)

-- we want to check forward and left. so for (-1,0) (up) that's adding (0,-1) (left)
-- for (1,0) (down) that's adding (0,1) (right)
robotPush : SortedSet (Int, Int) -> ((Int, Int), SortedSet (Int, Int)) -> (Int, Int) -> ((Int, Int), SortedSet (Int, Int))
robotPush walls (cur, blocks) dir =
    if (cur + dir) `contains` walls then (cur, blocks) else
    let neighbors: List (Int, Int) = map (+ cur) (neighboringPushLocations dir) -- forward, one forward-left
        neighborBlocks = filter (`contains` blocks) neighbors -- should only be one block in any of those two locations
        in
        (trace $ render2DMap (insert cur '@' (rerender blocks walls)) ++ "\n" ++ pack [directionOf' dir]) 
        $ case neighborBlocks of
            (neighborBlock :: []) => let (good,fn) = tryMove neighborBlock dir blocks walls in if good then ((cur + dir), fn blocks) else (cur, blocks)
            [] => (cur + dir, blocks)
            a => (trace $ "what " ++ show cur ++ " " ++ show dir ++ " " ++ show a) (cur + dir, blocks)

{-

    If the tile is #, the new map contains ## instead.
    If the tile is O, the new map contains [] instead.
    If the tile is ., the new map contains .. instead.
    If the tile is @, the new map contains @. instead.

 -}
modifyMap : List Char -> List Char
modifyMap ('#'::xs) = '#'::'#'::(modifyMap xs)
modifyMap ('O'::xs) = '['::']'::(modifyMap xs)
modifyMap ('.'::xs) = '.'::'.'::(modifyMap xs)
modifyMap ('@'::xs) = '@'::'.'::(modifyMap xs)
modifyMap (a::xs) = a::(modifyMap xs) -- shouldn't happen except maybe newlines
modifyMap [] = []

parseBlocksAndWalls : SortedMap (Int, Int) Char -> (SortedSet (Int, Int), SortedSet (Int, Int))
parseBlocksAndWalls m =
    let l: List ((Int, Int), Char) = toList m
        blocks = fromList $ map fst $ filter (\(k,v) => v == '[') l
        walls = fromList $ map fst $ filter (\(k,v) => v == '#') l in (blocks,walls)

partial part2 : String -> Int
part2 input =
    let (m' ::: (instructions' :: [])) = splitOn "" (lines input)
        m : SortedMap (Int, Int) Char = twoDStringToMap (pack $ modifyMap $ unpack (unlines m'))
        instructions'' : String = unlines instructions'
        instructions = map directionOf (unpack instructions'')
        (blocks,walls) = parseBlocksAndWalls m
        (ry,rx) : (Int, Int) = Builtin.fst $ (ne head) (filter (\(k,v) => v == '@') (toList m))
        (finalPos, finalBlocks) : ((Int, Int), SortedSet (Int, Int)) = foldl (\state, i => robotPush walls state i) ((ry,rx),blocks) instructions in (trace $ show (ry,rx) ++ "\n" ++ render2DMap (rerender blocks walls) ++ render2DMap (rerender finalBlocks walls)) 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2