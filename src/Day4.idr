module Day4

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities

-- Part 1

howManyInLine' : List Char -> Int
howManyInLine' ('X' :: 'M' :: 'A' :: 'S' :: xs) = 1 + howManyInLine' xs
howManyInLine' (c :: xs) = howManyInLine' xs
howManyInLine' [] = 0

howManyInLine : List Char -> Int
howManyInLine l = howManyInLine' l + howManyInLine' (reverse l)

horizontal : List (List Char) -> Int
horizontal lines = sum (map (howManyInLine) lines)

vertical : List (List Char) -> Int
vertical lines = horizontal (transpose lines)

getDiagonal : List (List a) -> List a
getDiagonal ((c :: chars) :: otherLists) = c :: getDiagonal (map (drop 1) otherLists)
getDiagonal _ = []

{-
Day4> getDiagonal [[1,2,3],[4,5,6],[7,8,9]]
[1, 5, 9]
 -}

-- drop first element of every row

getDiagonalsDroppingRows : List (List a) -> List (List a)
getDiagonalsDroppingRows (x :: xs) = getDiagonal (x :: xs) :: getDiagonalsDroppingRows xs
getDiagonalsDroppingRows [] = []

getDiagonalsDroppingColumns : List (List a) -> List (List a)
getDiagonalsDroppingColumns = getDiagonalsDroppingRows . transpose

getDiagonalsDroppingColumns' : (xs:(List (List a))) -> {auto _ : NonEmpty xs} -> List (List a)
getDiagonalsDroppingColumns' xs = case (getDiagonalsDroppingColumns xs) of
    (a :: bs) => bs
    _ => []

-- I NEED OPPOSITE DIAGS ALSO

cols : (xs:List (List a)) -> List (List a)
cols l = filter (\list => length list /= length l) $ getDiagonalsDroppingColumns l ++ getDiagonalsDroppingColumns (map reverse l)

rows : (xs:List (List a)) -> List (List a)
rows l = filter (\list => length list /= length l) $ getDiagonalsDroppingRows l ++ getDiagonalsDroppingRows (map reverse l)

majors : (xs:List (List a)) -> List (List a)
majors l = getDiagonal l :: [getDiagonal (map reverse l)]

part1 : String -> Int
part1 input =
    let theLines = map unpack (lines input) in
        case theLines of
            (_ :: _) =>
                let toConsider = (rows theLines) ++ (cols theLines) ++ (majors theLines) in (trace $ show (zip (rows theLines) (map howManyInLine (rows theLines))) ++ "\n" ++ show (zip (cols theLines) (map howManyInLine (cols theLines))) ++ " " ++ show (horizontal theLines) ++ " " ++ show (vertical theLines)) $ sum (map howManyInLine toConsider) + (horizontal theLines) + (vertical theLines)
            _ => 0

-- Not 2631 or 2630

{-
1 2 3
4 5 6
7 8 9
 -}

-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> Int
solve 0 = part1
solve 1 = part2