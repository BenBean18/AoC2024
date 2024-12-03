module Day3

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities

-- Part 1: initially done in frantic VS Code regex/python

part1 : String -> Int
part1 input = 1

-- Part 2

data Instruction a = Mul a a | Enable | Disable

Show (Instruction Int) where
    show (Mul a b) = show a ++ "*" ++ show b
    show Enable = "en"
    show Disable = "dis"

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
IM PARSING INPUTS BACKWARDS
mul(363,974)
[363*479
 -}

part2 : String -> Int
part2 input =
    let instrs = parseInstructions (unpack input) in (trace $ show instrs) execInstructions instrs True

public export
solve : Fin 2 -> String -> Int
solve 0 = part1
solve 1 = part2