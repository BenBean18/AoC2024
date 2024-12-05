module Day5

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.List1

-- Part 1

parseRule' : List Char -> (Nat, Nat)
parseRule' (c1 :: c2 :: '|' :: c4 :: c5 :: []) =
    let a : Nat = cast (pack [c1, c2])
        b : Nat = cast (pack [c4, c5]) in (a,b)
parseRule' _ = (0, 0)

parseRule : String -> (Nat, Nat)
parseRule = parseRule' . unpack

isValid : List (Nat, Nat) -> List Nat -> Bool
isValid ((a, b) :: rules) update = 
    let indexA = findIndex (==a) update
        indexB = findIndex (==b) update in
        (case indexA of
            (Just idxA) => case indexB of 
                (Just idxB) => idxA < idxB
                _ => True
            _ => True) && isValid rules update
isValid _ _ = True

parseUpdate : String -> List Nat
parseUpdate s = forget (map cast (map pack (splitOn ',' (unpack s))))

middle : List Nat -> Nat
middle l = case inBounds (((length l) `minus` 1) `div` 2) l of
    Yes prf => (((length l) `minus` 1) `div` 2) `index` l
    No _ => 0

r : List (Nat, Nat)
r = [(47, 53), (97, 13), (97, 61), (97, 47), (75, 29), (61, 13), (75, 53), (29, 13), (97, 29), (53, 29), (61, 53), (97, 53), (61, 29), (47, 13), (75, 47), (97, 75), (47, 61), (75, 61), (47, 29), (75, 13), (53, 13)]

part1 : String -> Int
part1 input = case forget (splitOn "" (lines input)) of
    rules' :: updates' :: [] => 
        let updates = map parseUpdate updates'
            rules = map parseRule rules'
            validUpdates = filter (isValid rules) updates in (trace $ show (length validUpdates)) cast (sum (map middle validUpdates))
    _ => 0

-- Part 2

-- 23! possibilities is not brute forceable lol

-- To fix an invalid one, we can just make a valid one
-- from the numbers and their applicable rules.

-- wait, this is actually just an Ordering? I think?
-- each rule defines a GT/LT relationship
-- but not really, because it has to look forward or backwards in the list

swapElements : Eq a => List a -> a -> a -> List a
swapElements (x :: l) a b = (if x == a then b else if x == b then a else x) :: swapElements l a b
swapElements [] _ _ = []

brokenRules : List (Nat, Nat) -> List Nat -> List (Nat, Nat)
brokenRules ((a, b) :: rules) update = 
    let indexA = findIndex (==a) update
        indexB = findIndex (==b) update in
        (case indexA of
            (Just idxA) => case indexB of 
                (Just idxB) => if idxA < idxB then [] else [(a,b)]
                _ => []
            _ => []) ++ brokenRules rules update
brokenRules _ _ = []

fix : List (Nat, Nat) -> List Nat -> List Nat
fix rules update = case brokenRules rules update of
    (a, b) :: _ => fix rules (swapElements update a b)
    [] => update

part2 : String -> Int
part2 input = case forget (splitOn "" (lines input)) of
    rules' :: updates' :: [] => 
        let updates = map parseUpdate updates'
            rules = map parseRule rules'
            invalidUpdates = filter (\u => not (isValid rules u)) updates
            fixedUpdates = map (fix rules) invalidUpdates in cast (sum (map middle fixedUpdates))
    _ => 0

public export
solve : Fin 2 -> String -> Int
solve 0 = part1
solve 1 = part2