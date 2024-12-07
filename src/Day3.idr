module Day3

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities

-- Part 2

-- Initial signature (Num a) => (b: a) -> Type resulted in
-- Num a -> Type, so a: Type that is numeric
-- b is an instance of type a
-- so Instruction 1 is valid, but Instruction Int is not
-- For this to work, a needs to be the Type of a Type (aka *)
-- and b needs to be a Type that has type a (aka the Type of a Type)
-- and Idris 2 doesn't have this yet (Cumulativity) :(

-- Dependent typing: going from values to types (up the hierarchy, e.g. from star to box)

-- We want Instruction : a -> Type, where there is an implementation of Num a
-- and a is a type
data Instruction : (a: Type) -> {auto p : Num a} -> Type where
    Mul : (x : a) -> (y : a) -> {auto p : Num a} -> Instruction a
    Enable : {auto p : Num a} -> Instruction a
    Disable : {auto p : Num a} -> Instruction a

-- works to show an instruction
(Show a) => (Num a) => Show (Instruction a) where
    show (Mul a b) = show a ++ "*" ++ show b
    show Enable = "en"
    show Disable = "dis"

-- doesn't work to show an instruction (no implementation for Instruction Integer)
-- (Show a, Num a) => Show (Instruction a) where
--     show instr = show' instr

parsePotentialMul : List Char -> List Char -> Bool -> List Char -> Maybe (Instruction Int)
parsePotentialMul (c :: text) currentNum1 num2Yet currentNum2 =
    if isDigit c then
        if num2Yet then parsePotentialMul text currentNum1 num2Yet (currentNum2 ++ [c])
        else parsePotentialMul text (currentNum1 ++ [c]) num2Yet currentNum2
    else if c == ',' then parsePotentialMul text currentNum1 True currentNum2
    else if c == ')' then Just (Mul ((the (String -> Int) cast) (pack currentNum1)) ((the (String -> Int) cast) (pack currentNum2)))
    else Nothing
parsePotentialMul [] _ _ _ = Nothing

parseInstructions : List Char -> List (Instruction Int)
parseInstructions ('m' :: 'u' :: 'l' :: '(' :: xs) =
    let maybeMul = parsePotentialMul xs [] False [] in
        case maybeMul of
            Just mul => mul :: parseInstructions xs
            Nothing => parseInstructions xs
parseInstructions ('d' :: 'o' :: '(' :: ')' :: xs) = Enable :: parseInstructions xs
parseInstructions ('d' :: 'o' :: 'n' :: '\'' :: 't' :: '(' :: ')' :: xs) = Disable :: parseInstructions xs
parseInstructions (_ :: xs) = parseInstructions xs
parseInstructions [] = []

execInstructions : List (Instruction Int) -> Bool -> Int
execInstructions ((Mul a b) :: xs) en = (if en then (a * b) else 0) + (execInstructions xs en)
execInstructions (Enable :: xs) en = execInstructions xs True
execInstructions (Disable :: xs) en = execInstructions xs False
execInstructions [] _ = 0

{-
I WAS PARSING INPUTS BACKWARDS
mul(363,974)
[363*479
 -}

part2 : String -> Int
part2 input =
    let instrs = parseInstructions (unpack input) in (trace $ show instrs) execInstructions instrs True

-- Part 1: initially done in frantic VS Code regex/python

execInstructions1 : List (Instruction Int) -> Bool -> Int
execInstructions1 ((Mul a b) :: xs) en = (if en then (a * b) else 0) + (execInstructions1 xs en)
execInstructions1 (Enable :: xs) en = execInstructions1 xs True
execInstructions1 (Disable :: xs) en = execInstructions1 xs True
execInstructions1 [] _ = 0

part1 : String -> Int
part1 input = execInstructions1 (parseInstructions (unpack input)) True

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2