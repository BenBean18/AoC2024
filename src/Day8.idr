module Day8

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.SortedMap

-- Part 1: 16.908ms

antinodesOfPair : ((Int, Int),(Int, Int)) -> List (Int, Int)
antinodesOfPair (p1,p2) = 
    let dif = p2 + p1 in [p2 - dif, p1 - dif]

-- [1,2,3,4] --> [1,(2,3,4)], [2,(3,4)], [3,4]

pairs : Ord a => List a -> List (a,a)
pairs (x :: xs) = map (\i => (x,i)) xs ++ pairs xs
pairs _ = []

-- would be cool to write this as a vect and verify output length is n `choose` 2

antinodesOfResonantAntennas : List (Int, Int) -> List (Int, Int)
antinodesOfResonantAntennas l = 
    let antennaPairs = pairs l in concatMap antinodesOfPair antennaPairs

findResonantAntennaGroups : SortedMap (Int, Int) Char -> SortedMap Char (List (Int, Int))
findResonantAntennaGroups m =
    let uniqueValues = filter (/= '.') $ nub $ values m
        listVersion : List ((Int, Int), Char) = toList m in fromList $ map (\v => (v, map fst $ filter (\(k1,v1) => v1 == v) listVersion)) uniqueValues

-- elem : Eq a => List a -> a -> Bool
-- elem (x :: xs) b = if x == b then True else elem xs b
-- elem [] _ = False

{-
stdlib is much cleaner lol
elemBy : Foldable t => (a -> a -> Bool) -> a -> t a -> Bool
elemBy p e = any (p e)
 -}

findAllAntinodes : SortedMap (Int, Int) Char -> List (Int, Int)
findAllAntinodes m =
    let resonantAntennaGroups = findResonantAntennaGroups m
        allAntinodes = concatMap antinodesOfResonantAntennas resonantAntennaGroups
        locationsInMap = keys m in filter (\l => l `elem` allAntinodes) locationsInMap

part1 : String -> Int
part1 input =
    let m = twoDStringToMap input in cast $ length (findAllAntinodes m)

-- Part 2: 313.331ms

-- https://stackoverflow.com/a/7922967
-- Euclidean algorithm
gcd : Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

reduce : (Int,Int) -> (Int,Int)
reduce (0,0) = (0,0) -- no div by 0
reduce (a,b) = let g = gcd (abs a) (abs b) in ((a `div` g), (b `div` g))

-- I FORGOT THE OPPOSITE WAS VALID TOO
-- UGH
-- e.g. (x,y) and (-x,-y) are both valid

linearAntinodesOfPair : ((Int, Int),(Int, Int)) -> ((Int, Int) -> Bool)
linearAntinodesOfPair (p1,p2) = 
    let dif = p2 - p1 in (\(a,b) => reduce dif == reduce ((a,b) - p1) || ((0,0) - (reduce dif)) == reduce ((a,b) - p1) || reduce ((a,b) - p1) == (0,0))

linearAntinodesOfResonantAntennas : List (Int, Int) -> List (Int, Int) -> List (Int, Int)
linearAntinodesOfResonantAntennas allLocations as = 
    let p = pairs as
        checker : (Int, Int) -> Bool = (\loc => any (\t => linearAntinodesOfPair t loc) p) in filter checker allLocations

countAllLinearAntinodes : SortedMap (Int, Int) Char -> Int
countAllLinearAntinodes m =
    let resonantAntennaGroups = findResonantAntennaGroups m
        allAntinodes = nub $ concatMap (linearAntinodesOfResonantAntennas (keys m)) resonantAntennaGroups in cast $ length $ allAntinodes

part2 : String -> Int
part2 input =
    let m = twoDStringToMap input in countAllLinearAntinodes m

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2