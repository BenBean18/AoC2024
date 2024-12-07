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
            validUpdates = filter (isValid rules) updates in cast (sum (map middle validUpdates))
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

-- this is a weird "sorting" algorithm
fix_old : List (Nat, Nat) -> List Nat -> List Nat
fix_old rules update = case brokenRules rules update of
    (a, b) :: _ => fix_old rules (swapElements update a b)
    [] => update

part2_old : String -> Int
part2_old input = case forget (splitOn "" (lines input)) of
    rules' :: updates' :: [] => 
        let updates = map parseUpdate updates'
            rules = map parseRule rules'
            invalidUpdates = filter (\u => not (isValid rules u)) updates
            fixedUpdates = map (fix_old rules) invalidUpdates in cast (sum (map middle fixedUpdates))
    _ => 0

-- Swapping version: 385.275ms

-------------------------------------------------------------------------------

-- actually, the comparison way does work, because otherwise sorting normally doesn't. i think?
-- when looking at other solutions i saw someone do it so i'll try lol

data Page : List (Nat, Nat) -> Type where
    Pg : Nat -> Page rules

Eq (Page rules) where
    (==) (Pg a) (Pg b) = a == b

-- bruh i just spent like 10 minutes to make Idris stop complaining
-- error: "rules is not accessible in this context."
-- fix: `{rules : List (Nat, Nat)} -> `
{rules : List (Nat, Nat)} -> Ord (Page rules) where
    (<) (Pg a) (Pg b) = length (filter (==(b,a)) rules) == 0

{-
If Idris used bubble sort, I think this wouldn't work.

Bubble sort swaps adjacent elements if they're misordered.
75,97,47,61,53 breaks 97|75, and bubble sort catches it because
they're adjacent.

Bubble sort would not fix 75,47,97,61,53 because 75 and 97
are not adjacent.

But Idris uses mergesort, and when I tested that second example, it 
was successfully fixed.

Just did some wikipediaing, this is **exactly** what we're dealing with!
https://en.wikipedia.org/wiki/Partially_ordered_set

https://arxiv.org/pdf/0707.1532v1

/shrug going to sleep but will think about this more

Topological sort is apparently the right way to do this

It's O(V + E)

(although if you use it to sort a fully ordered set i.e. (Fin n),
time complexity is O(n + n `choose` 2) = O(n!), since it's effectively
sorting the directed complete graph on n vertices)

this is really cool and just makes sense lol
https://en.wikipedia.org/wiki/Topological_sorting#Kahn's_algorithm
 -}
fix : {auto rules : List (Nat, Nat)} -> List (Page rules) -> List (Page rules)
fix update = sort update

natToPage : {auto rules : List (Nat, Nat)} -> Nat -> Page rules
natToPage n = Pg n

pageToNat : Page _ -> Nat
pageToNat (Pg n) = n

part2 : String -> Int
part2 input = case forget (splitOn "" (lines input)) of
    rules' :: updates' :: [] => 
        let updates = map parseUpdate updates'
            rules = map parseRule rules'
            invalidUpdates' = filter (\u => not (isValid rules u)) updates
            invalidUpdates = map (map natToPage) invalidUpdates' -- AUTO DETECTING RULES WHAT THE HECK
            fixedUpdates' = map fix invalidUpdates
            fixedUpdates = map (map pageToNat) fixedUpdates' in cast (sum (map middle fixedUpdates))
    _ => 0

-- I LOVE DEPENDENT TYPING
-- Optimized version: 27.353ms (14x speedup!!)
-- ...because you're going from O(n^2) to whatever the std library has
-- :upside_down:

-- This can apparently be done in **linear** time https://rcoh.me/posts/linear-time-median-finding/

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2