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

-- Part 1

-- we building a VM! :)

record Registers where
    constructor MkRegisters
    A, B, C, IP : Int

-- stores the output as a string as well as the current state
-- bind operation: (>>=)  : m a -> (a -> m b) -> m b
-- (>>=) : WithOutput Registers -> (Registers -> WithOutput Registers) -> WithOutput Registers
-- Initializer: MkOutput "the output" CurrentState -> WithOutput StateType
data WithOutput : (state : Type) -> Type where
    MkOutput : String -> state -> WithOutput state

combineStrings : WithOutput a -> WithOutput b -> WithOutput b
combineStrings (MkOutput s1 _) (MkOutput s2 state2) = MkOutput (s1 ++ "," ++ s2) state2

Functor WithOutput where
    map f (MkOutput s state) = MkOutput s (f state)

Applicative WithOutput where
    -- pure  : state -> WithOutput state
    -- if we just have a state, output should start as nothing
    pure state = MkOutput "" state

    -- (<*>) : WithOutput (stateType1 -> stateType2) -> WithOutput stateType1 -> WithOutput stateType2
    -- this kinda doesn't make any sense given the context, but uh ig just concatenate the outputs?
    -- we'll only ever be wrapping one type (registers)
    (MkOutput s1 f) <*> (MkOutput s2 a) = MkOutput (s1 ++ "," ++ s2) (f a)

Monad WithOutput where
    -- (>>=)  : WithOutput stateType1 -> (stateType1 -> WithOutput stateType2) -> WithOutput stateType2
    -- want to combine the strings
    a@(MkOutput s1 state) >>= f = combineStrings a (f state)

data Parity : Nat -> Type where
    Even : {n : _} -> Parity (n + n)
    Odd  : {n : _} -> Parity (S (n + n))

-- force even number
Program : Nat -> Type
Program n = Vect (n + n) (Fin 8)

-- lemma : (left : Nat) -> (right : Nat) -> Vect (S (S (left + right))) (Fin 8) = Vect (S (left + S right)) (Fin 8)
-- lemma left right = rewrite plusSuccRightSucc left right in Refl

lemma1 : (k : Nat) -> k + S k = S (k + k)
lemma1 k = rewrite plusSuccRightSucc k k in Refl

lemma2 : (k : Nat) -> S k + S k = S ((S k) + k)
lemma2 k = rewrite plusSuccRightSucc k k in Refl

lemma3 : (k : Nat) -> S k + S k = S (k + (S k))
lemma3 k = rewrite lemma2 k in Refl

firstTwo : {k : Nat} -> Fin 8 -> Fin 8 -> Program k -> Program (S k)
firstTwo a b xs =
    let toAdd : Program 1 = (a :: b :: [])
        -- Can't solve constraint between: S (plus 0 (k + k)) and plus k (S k)
        result : Vect (S (S (k + k))) (Fin 8) = toAdd ++ xs
        result' : Vect (S (k + (S k))) (Fin 8) = rewrite lemma1 k in result
        result'' : Vect (S k + S k) (Fin 8) = rewrite lemma3 k in result' in result''

run : {n : Nat} -> Registers -> Program n -> WithOutput Registers
run {n=0} r [] = MkOutput "" r
-- adv (division a)
run {n=(S k)} (MkRegisters a b c ip) (firstTwo 0 combo xs) = ?hole
-- -- bxl (bitwise or)
-- run {n=(S k)} (MkRegisters a b c ip) (1::literal::xs) = run (MkRegisters a b c ip) xs
-- -- bst (mod 8)
-- run {n=(S k)} (MkRegisters a b c ip) (2::combo::xs) = run (MkRegisters a b c ip) xs
-- -- jnz (jump if nonzero)
-- run {n=(S k)} (MkRegisters a b c ip) (3::literal::xs) = run (MkRegisters a b c ip) xs
-- -- bxc (bitwise or)
-- run {n=(S k)} (MkRegisters a b c ip) (4::_::xs) = run (MkRegisters a b c ip) xs
-- -- out
-- run {n=(S k)} (MkRegisters a b c ip) (5::combo::xs) = run (MkRegisters a b c ip) xs
-- -- bdv (division b)
-- run {n=(S k)} (MkRegisters a b c ip) (6::combo::xs) = run (MkRegisters a b c ip) xs
-- -- cdv (division c)
-- run {n=(S k)} (MkRegisters a b c ip) (7::combo::xs) = run (MkRegisters a b c ip) xs

part1 : String -> Int
part1 input = 1

-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2