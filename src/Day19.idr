module Day19

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
import Control.Monad.ST

-- Part 1

-- this feels like combinatorics
-- idk let's try brute force
-- wait this is less easy than i thought, there can be multiple ways to have a prefix
-- uhhhhhh
-- it's definitely recursive

-- hmm works on sample but actual might not be brute forceable

-- it is calling the same function on the same value many times, so we know what that means

r : (Ord k) => ST s (STRef s (SortedMap k v))
r = newSTRef (Data.SortedMap.empty)

memoize' : (Show a) => (Ord a) => (a -> b) -> a -> ST s b
memoize' f input = do
    memoRef <- r
    memo <- readSTRef memoRef
    (trace $ show (length $ Data.SortedMap.toList memo)) $ case lookup input memo of
        Just output => (trace "found") pure output
        Nothing => do
            let output = f input
            (trace $ "inserted " ++ show input) $ modifySTRef memoRef (insert input output)
            pure output

isPossible : List String -> String -> SortedMap String Bool -> (Bool, SortedMap String Bool)
isPossible l "" memo = (True, memo)
isPossible l str memo = (trace $ show (length $ Data.SortedMap.toList memo)) $
    case lookup str memo of
        Just b => (b,memo)
        Nothing =>
            let possibles = filter (`isPrefixOf` str) l
                trimmed = map (\p => pack (drop (length p) (unpack str))) possibles
                (result,newMemo) = foldl (\(done,currentMemo), elem => if done then (True,currentMemo) else isPossible l elem currentMemo) (False,memo) trimmed in
                    (result, insert str result newMemo)

partial parseInput : List String -> (List String, List String)
parseInput (patterns::_::rest) = (filter (/= "") (forget (split (\a => a == ',' || a == ' ') patterns)), rest)

partial part1 : String -> Int
part1 input =
    let (patterns, designs) = parseInput (lines input)
        m : SortedMap String Bool = empty
        results : (Int,SortedMap String Bool) = foldl (\(count,currentMemo), d => let (t,thisMemo) = isPossible patterns d currentMemo in (count + if t then 1 else 0, mergeLeft thisMemo currentMemo)) (the Int 0,m) designs in fst results

-- Part 2

part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2