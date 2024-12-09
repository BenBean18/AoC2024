module Day9

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.SnocList
import Data.List1

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

-- Block Id Length, Empty Length
data DiskEntry2 = Block2 Int Int | Empty2 Int

Disk2 : Type
Disk2 = List DiskEntry2

parseDisk2' : List Char -> Int -> Disk2
parseDisk2' (fileSize :: emptySize :: xs) i = [Block2 i (cast (cast fileSize `minus` (cast '0')))] ++ (if (cast (cast emptySize `minus` (cast '0'))) > 0 then [Empty2 (cast (cast emptySize `minus` (cast '0')))] else []) ++ parseDisk2' xs (i+1)
parseDisk2' (fileSize :: []) i = [Block2 i (cast (cast fileSize `minus` (cast '0')))]
parseDisk2' [] _ = []

parseDisk2 : List Char -> Disk2
parseDisk2 = (flip parseDisk2') 0

emptiesOnly : Disk2 -> List Int
emptiesOnly ((Empty2 i) :: xs) = i :: emptiesOnly xs
emptiesOnly ((Block2 _ _) :: xs) = emptiesOnly xs
emptiesOnly [] = []

largestEmpty : Disk2 -> Int
largestEmpty d = let s = sort (emptiesOnly d) in case s of
    (_ :: _) => last s
    _ => 0

{-
00...111...2...333.44.5555.6666.777.888899
0099.111...2...333.44.5555.6666.777.8888..
0099.1117772...333.44.5555.6666.....8888..
0099.111777244.333....5555.6666.....8888..
00992111777.44.333....5555.6666.....8888..
 -}

Show DiskEntry2 where
    show (Block2 idx len) = pack (replicate (cast len) (cast (idx + cast '0')))
    show (Empty2 len) = pack (replicate (cast len) '.')

Eq DiskEntry2 where
    (==) (Block2 a b) (Block2 c d) = a == c && b == d
    (==) (Empty2 a) (Empty2 b) = a == b
    (==) (Empty2 _) (Block2 _ _) = False
    (==) (Block2 _ _) (Empty2 _) = False

putInSpace : Disk2 -> Int -> Int -> Disk2
putInSpace (Empty2 emptyLen :: xs) blkId len = 
    let dif = emptyLen - len in if dif >= 0 then
        (Block2 blkId len) :: (if dif > 0 then (Empty2 (emptyLen - len)) :: xs else xs)
        else (Empty2 emptyLen) :: putInSpace xs blkId len
putInSpace (Block2 a b :: xs) blkId len = (Block2 a b) :: putInSpace xs blkId len
putInSpace [] _ _ = []

-- Potential optimization: once a block doesn't fit, don't keep trying to fit it
-- These add up, I can see the runtime getting slower over time

combineEmpty : DiskEntry2 -> DiskEntry2 -> DiskEntry2
combineEmpty (Empty2 a) (Empty2 b) = Empty2 (a + b)
combineEmpty _ _ = Empty2 0

removeTrailing : Disk2 -> (Disk2, Disk2)
removeTrailing dsk = let    splitBlock = filter (\a => case a of
                                                (Block2 i _) => True
                                                (Empty2 _) => False) dsk
                            lastBlock = last @{believe_me (NonEmpty splitBlock)} splitBlock
                            splitEmpties = forget (split (\a => a == lastBlock) dsk)
                            empties: Disk2 = (last @{believe_me (NonEmpty splitEmpties)} splitEmpties)
                            combined: DiskEntry2 = foldl combineEmpty (Empty2 0) empties
                            
                          in ((concat (init @{believe_me (NonEmpty splitEmpties)} splitEmpties)) ++ [lastBlock] ++ [combined], (last @{believe_me (NonEmpty splitEmpties)} splitEmpties))


-- move last empty into first free
-- and subtract number from free available
-- recurse on init
fillSpace2' : (Disk2, Int) -> Bool -> (Disk2, Int)
fillSpace2' (dsk',maxUsefulID) f = let dsk: Disk2 = if f then (fst $ removeTrailing dsk') else dsk' in case dsk of
    (a :: bs) =>
        let lastOne = last (a::bs) in case lastOne of
            (Block2 blkId len) => let newDsk = putInSpace dsk blkId len in {-(trace $ "hi " ++ show newDsk)$-} case newDsk of
                    (c :: ds) => if newDsk /= dsk then 
                        (trace $ "Moved " ++ show blkId ++ " " ++ show len ++ ", ignoring >=" ++ show maxUsefulID) $ 
                        (init (c::ds) ++ [Empty2 len], maxUsefulID) -- could move the block, we're good, it is NOT immovable, continue on (this was blkId-1 which was wrong)
                        else
                        (trace $ "Couldn't move " ++ show blkId ++ " " ++ show len) $ 
                        -- could NOT move the block
                        -- set maximum usable id to the ID of this block
                        let (l, i) = fillSpace2' (init (a::bs),blkId) False in (l ++ [lastOne] ++ [], i) -- this was maxUsefulID instead of blkId which was wrong
                        -- we only want to reset id when we can't move something
                    [] => ([],maxUsefulID)
            Empty2 _ => let (l, i) = fillSpace2' (init (a::bs),maxUsefulID) False in (l ++ [lastOne], i)
    _ => ([],maxUsefulID)

pd : Disk2
pd = parseDisk2' (unpack example) 0

{-
f***
Exception: invalid memory reference.  Some debugging context lost
Error: invalid memory reference
()
 -}

trim : (Disk2, Int) -> (Disk2, Disk2, Disk2, Int)
trim (dsk,maxUsefulID) = --(trace $ "trim " ++ show dsk ++ " " ++ show maxUsefulID) $
                         let splitEmpties = forget (split (\a => case a of
                                                (Block2 i _) => False
                                                (Empty2 _) => True) dsk) 
                             (x ::: xs) = (split (\a => case a of
                                                (Block2 i _) => i == maxUsefulID
                                                (Empty2 _) => False) dsk)
                             splitBlock = filter (\a => case a of
                                                (Block2 i _) => i == maxUsefulID
                                                (Empty2 _) => False) dsk 
                             in 
                                                    case xs of
                                                        (f :: _) => 
                                                            case splitBlock of 
                                                                (b :: _) => (x,f,[b],maxUsefulID)
                                                                _ => (x,f,[],maxUsefulID)
                                                        _ => (x,[],[],maxUsefulID)

-- 9736426581287 too high
-- 6325313007183 is too low
-- 6327174563252 LETS F***ING GOOOOOOO
-- right before I was going to go to bed I realized combining trailing whitespace is always valid

-- the problem is fillSpace2' needs to keep searching further and further to find the first place to be effective
-- but we can't always remove trailing whitespace since it's recursive, so we can only remove trailing whitespace when...
-- it's the first time calling fillSpace2' i think?

fillSpace2 : (Disk2, Int) -> (Disk2, Int)
fillSpace2 (dsk,maxUsefulID) = (trace $ "l" ++ show (length dsk)) $
    let (next,also,splitBlock,newID) = trim (fillSpace2' (dsk,maxUsefulID) True) in  --(trace $ show dsk ++ "->" ++ show next ++ " " ++ show splitBlock ++ " " ++ show also) $
    case next of
        (x::y) => if dsk == next then (next ++ splitBlock ++ also,newID) else --(trace $ "ignoring " ++ show (last (x::y))) $ 
            let (nextIterDsk, nextIterMaxID) = fillSpace2 (x::y, newID) in {-(trace $ "reassembling " ++ show nextIterDsk ++ " " ++ show splitBlock ++ " " ++ show also) $-} (nextIterDsk ++ splitBlock ++ also, newID)
        _ => (dsk,maxUsefulID) -- WAS THAT REALLY IT THAT THIS SHOULD BE DSK NOT []

convertBack : Disk2 -> Disk
convertBack ((Block2 blkId len) :: xs) = (replicate (cast len) (Block blkId)) ++ convertBack xs
convertBack ((Empty2 len) :: xs) = (replicate (cast len) Empty) ++ convertBack xs
convertBack [] = []

t : IO ()
t = printLn (checksum (convertBack (fst $ fillSpace2 (pd,1000000))))

--  fillSpace2 (fillSpace2 (fillSpace2 (fillSpace2 (fillSpace2 (fillSpace2 pd))))))

-- exit condition: largest free space block < last entry

part2 : String -> Int
part2 input = let parsed = (parseDisk2' (unpack input) 0)
                  theEnd = (fst $ fillSpace2 (parsed, 100000)) in
    (trace $ show theEnd) $ case parsed of
        (_ :: _) => checksum (convertBack theEnd)
        _ => 0

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2