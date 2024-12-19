module Day17

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
import Data.Bits

-- Part 1

-- we building a VM! :)

record Registers where
    constructor MkRegisters
    A, B, C, IP : Int -- ideally IP is Fin (program length), but Fins are slow

-- stores the output as a string as well as the current state
-- bind operation: (>>=)  : m a -> (a -> m b) -> m b
-- (>>=) : WithOutput Registers -> (Registers -> WithOutput Registers) -> WithOutput Registers
-- Initializer: MkOutput "the output" CurrentState -> WithOutput StateType
data WithOutput : (state : Type) -> Type where
    MkOutput : String -> state -> WithOutput state

NoOutput : {state : Type} -> state -> WithOutput state
NoOutput = MkOutput ""

combineStrings : WithOutput a -> WithOutput b -> WithOutput b
combineStrings (MkOutput s1 _) (MkOutput s2 state2) = MkOutput (s1 ++ s2) state2

Functor WithOutput where
    map f (MkOutput s state) = MkOutput s (f state)

Applicative WithOutput where
    -- pure  : state -> WithOutput state
    -- if we just have a state, output should start as nothing
    pure state = MkOutput "" state

    -- (<*>) : WithOutput (stateType1 -> stateType2) -> WithOutput stateType1 -> WithOutput stateType2
    -- this kinda doesn't make any sense given the context, but uh ig just concatenate the outputs?
    -- we'll only ever be wrapping one type (registers)
    (MkOutput s1 f) <*> (MkOutput s2 a) = MkOutput (s1 ++ s2) (f a)

Monad WithOutput where
    -- (>>=)  : WithOutput stateType1 -> (stateType1 -> WithOutput stateType2) -> WithOutput stateType2
    -- want to combine the strings
    a@(MkOutput s1 state) >>= f = combineStrings a (f state)

parseCombo : Registers -> Fin 8 -> Int
parseCombo (MkRegisters a b c ip) 7 = 0
parseCombo (MkRegisters a b c ip) 6 = c
parseCombo (MkRegisters a b c ip) 5 = b
parseCombo (MkRegisters a b c ip) 4 = a
parseCombo (MkRegisters a b c ip) k = cast (finToNat k)

-- i **reallllly** wanted to do this with a traverse
-- but jumping prevents doing that, because we occasionally need to drop things from the list
-- OH FRICK THE INSTRUCTION POINTER COULD GO BACKWARDS FROM CURRENT
-- uhhhhhhhh
-- yippee
-- now run will just run a singular instruction
run : Registers -> Vect 2 (Fin 8) -> WithOutput Registers
-- adv (division a)
-- this is a bitshift right: "The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand."
run r@(MkRegisters a b c ip) (0::combo::[]) = NoOutput (MkRegisters (a `shiftR` (restrict 63 (cast (parseCombo r combo)))) b c (ip + 2))
-- run r@(MkRegisters a b c ip) (0::combo::xs) = do
--     nextState <- NoOutput (MkRegisters (a `shiftR` (restrict 63 (cast (parseCombo r combo)))) b c (ip + 2))
--     run nextState xs
-- bxl (bitwise or)
run r@(MkRegisters a b c ip) (1::literal::[]) = NoOutput (MkRegisters a (b `xor` (cast (finToInteger literal))) c (ip + 2))
-- bst (mod 8)
run r@(MkRegisters a b c ip) (2::combo::[]) = NoOutput (MkRegisters a ((parseCombo r combo) `mod` 8) c (ip + 2))
-- jnz (jump if nonzero)
run r@(MkRegisters a b c ip) (3::literal::[]) = case a of
    0 => NoOutput (MkRegisters a b c (ip + 2))
    a => NoOutput (MkRegisters a b c (cast (finToInteger literal)))
-- bxc (bitwise or)
run r@(MkRegisters a b c ip) (4::_::[]) = NoOutput (MkRegisters a (b `xor` c) c (ip + 2))
-- out (mod 8)
run r@(MkRegisters a b c ip) (5::combo::[]) = MkOutput (show ((parseCombo r combo) `mod` 8) ++ ",") (MkRegisters a b c (ip + 2))
-- bdv (division b)
run r@(MkRegisters a b c ip) (6::combo::[]) = NoOutput (MkRegisters a (a `shiftR` (restrict 63 (cast (parseCombo r combo)))) c (ip + 2))
-- cdv (division c)
run r@(MkRegisters a b c ip) (7::combo::[]) = NoOutput (MkRegisters a b (a `shiftR` (restrict 63 (cast (parseCombo r combo)))) (ip + 2))

runProgram : List Int -> Registers -> WithOutput Registers
runProgram xs r@(MkRegisters a b c ip) = do
    case (map (restrict 7 . cast) $ take 2 (drop (cast ip) xs)) of
        (a :: b :: _) => do
            nextState <- run r (a::b::[])
            runProgram xs nextState
        -- (a :: b :: _) => run r (a::b::[]) >>= runProgram xs
        [_] => NoOutput r
        [] => NoOutput r

partial parseInput : List String -> (Registers, List Int)
parseInput (a::b::c::_::program::xs) = 
    let aVal : Int = cast ((ne last) (words a))
        bVal : Int = cast ((ne last) (words b))
        cVal : Int = cast ((ne last) (words c))
        prgm : List Int = map cast (forget (split (== ',') program)) in (MkRegisters aVal bVal cVal 0, prgm)

partial part1 : String -> IO Int
part1 input =
    let (regs,prgm) = parseInput (lines input)
        (MkOutput out _) = runProgram prgm regs in do
        putStrLn $ pack $ (ne init) (unpack out)
        pure 0

-- Part 2

part2 : String -> Int
part2 input = 2

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = part1
solve 1 = pure . part2