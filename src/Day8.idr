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

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2