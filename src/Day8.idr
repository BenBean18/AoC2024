module Day8

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities
import Data.SortedMap

-- Part 1

s : (Int, Int) -> (Int, Int) -> (Int, Int)
s (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

m : (Int, Int) -> (Int, Int) -> (Int, Int)
m (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

antinodesOf' : ((Int, Int),(Int, Int)) -> List (Int, Int)
antinodesOf' (p1,p2) = 
    let dif = p2 `m` p1 in [p2 `s` dif, p1 `m` dif]

-- [1,2,3,4] --> [1,(2,3,4)], [2,(3,4)], [3,4]

pairs : Ord a => List a -> List (a,a)
pairs (x :: xs) = map (\i => ((min x i), (max x i))) xs ++ pairs xs
pairs _ = []

antinodesOf : List (Int, Int) -> List (Int, Int)
antinodesOf l = 
    let p = pairs l in concatMap antinodesOf' p

findAntennas : SortedMap (Int, Int) Char -> SortedMap Char (List (Int, Int))
findAntennas m =
    let uniqueValues = filter (/= '.') $ nub $ values m
        listVersion : List ((Int, Int), Char) = toList m in fromList $ map (\v => (v, map fst $ filter (\(k1,v1) => v1 == v) listVersion)) uniqueValues

-- elem : Eq a => List a -> a -> Bool
-- elem (x :: xs) b = if x == b then True else elem xs b
-- elem [] _ = False

findAntinodes : SortedMap (Int, Int) Char -> List (Int, Int)
findAntinodes m =
    let antennas = findAntennas m
        antinodes = concatMap antinodesOf antennas
        mapLocations = keys m in filter (\l => l `elem` antinodes) mapLocations

part1 : String -> Int
part1 input =
    let m = twoDStringToMap input in cast $ length (findAntinodes m)

-- Part 2

-- allAntennas : SortedMap (Int, Int) Char -> List (Int, Int)
-- allAntennas m =
--     let antennas = map (\(a,b)=>a) $ filter (\(k,v) => v /= '.') $ (Data.SortedMap.toList m) in antennas

-- findAntinodes2 : SortedMap (Int, Int) Char -> List (Int, Int)
-- findAntinodes2 m =
--     let antennas = allAntennas m
--         antinodes = concatMap antinodesOf [antennas]
--         mapLocations = keys m in filter (\l => l `elem` antinodes) mapLocations

-- https://stackoverflow.com/a/7922967
-- Euclidean algorithm
gcd : Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b (a `mod` b)

reduce : (Int,Int) -> (Int,Int)
reduce (0,0) = (0,0) -- no div by 0
reduce (a,b) = let g = gcd (abs a) (abs b) in ((a `div` g), (b `div` g))

-- I FORGOT THE RECIPROCAL WAS VALID TOO
-- UGH

antinodesOf2' : ((Int, Int),(Int, Int)) -> ((Int, Int) -> Bool)
antinodesOf2' (p1,p2) = 
    let dif = p2 `m` p1 in (\(a,b) => reduce dif == reduce ((a,b) `m` p1) || ((0,0) `m` (reduce dif)) == reduce ((a,b) `m` p1) || reduce ((a,b) `m` p1) == (0,0))

countAntinodesOf2 : List (Int, Int) -> List (Int, Int) -> List (Int, Int)
countAntinodesOf2 allLocations as = 
    let p = pairs as
        checker : (Int, Int) -> Bool = (\loc => any (\t => antinodesOf2' t loc) p) in filter checker allLocations

countAllAntinodes2 : SortedMap (Int, Int) Char -> Int
countAllAntinodes2 m =
    let antennas = findAntennas m
        antinodes = nub $ concatMap (countAntinodesOf2 (keys m)) antennas in (trace $ show (findAntennas m) ++ "\n" ++ show antinodes) cast $ length $ antinodes

part2 : String -> Int
part2 input =
    let m = twoDStringToMap input in countAllAntinodes2 m

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2