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
                let toConsider = (rows theLines) ++ (cols theLines) ++ (majors theLines) in {-(trace $ show (zip (rows theLines) (map howManyInLine (rows theLines))) ++ "\n" ++ show (zip (cols theLines) (map howManyInLine (cols theLines))) ++ " " ++ show (horizontal theLines) ++ " " ++ show (vertical theLines)) $ -}sum (map howManyInLine toConsider) + (horizontal theLines) + (vertical theLines)
            _ => 0

-- Not 2631 or 2630

{-
1 2 3
4 5 6
7 8 9
 -}

-- Part 2

-- Store coordinates with matches
pairWithIndices' : List a -> Nat -> List (Nat, a)
pairWithIndices' (x :: xs) idx = (idx, x) :: pairWithIndices' xs (S idx)
pairWithIndices' [] _ = []

pairWithIndices : List a -> List (Nat, a)
pairWithIndices l = pairWithIndices' l Z

pair2DWithIndices : List (List a) -> List (List ((Nat, Nat), a))
pair2DWithIndices l = map (\(rowIdx, row) => map (\(colIdx, cell) => ((rowIdx, colIdx), cell)) (pairWithIndices row)) (pairWithIndices l)

findMAS' : List ((Nat, Nat), Char) -> List (Nat, Nat)
findMAS' (((rM, cM), 'M') :: ((rA, cA), 'A') :: ((rS, cS), 'S') :: xs) = (rA, cA) :: findMAS' xs
findMAS' (c :: xs) = findMAS' xs
findMAS' [] = []

-- MUST BE SORTED
countDupes' : Eq a => List a -> Int
countDupes' (a :: b :: xs) =
    if a == b then 1 + countDupes' xs else countDupes' (b :: xs)
countDupes' _ = 0

countDupes : Ord a => Eq a => List a -> Int
countDupes = countDupes' . sort

findMAS : List ((Nat, Nat), Char) -> List (Nat, Nat)
findMAS l = findMAS' l ++ findMAS' (reverse l)

countMAS : List (List ((Nat, Nat), Char)) -> Int
countMAS l = countDupes (concat (map findMAS l))

part2 : String -> Int
part2 input =
    let theLines = pair2DWithIndices (map unpack (lines input)) in
    case theLines of
        (_ :: _) =>
            let diagsToConsider = (rows theLines) ++ (cols theLines) ++ (majors theLines) in {-(trace $ show diagsToConsider ++ "\n\n\n" ++ show (map findMAS diagsToConsider) ++ "\n\n\n" ++ show (countDupes (map findMAS diagsToConsider))) $-} countMAS diagsToConsider
        _ => 0

{-
1 2 3
. 4 5 6
. . 7 8 9
 -}

{-
1 4 7
. 2 5 8
. . 3 6 9
 -}

public export
solve : Fin 2 -> String -> Int
solve 0 = part1
solve 1 = part2

-- visualization
-- putStr ("\x1b" ++ "c") -- clears screen, \x1b is ANSI escape
-- ideally make MASes yellow then highlight As
visFindMAS' : List ((Nat, Nat), Char) -> List (Nat, Nat)
visFindMAS' (((rM, cM), 'M') :: ((rA, cA), 'A') :: ((rS, cS), 'S') :: xs) = (rA, cA) :: findMAS' xs
visFindMAS' (c :: xs) = findMAS' xs
visFindMAS' [] = []