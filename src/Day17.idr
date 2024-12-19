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
    A, B, C, IP : Int

-- stores the output as a string as well as the current state
-- bind operation: (>>=)  : m a -> (a -> m b) -> m b
-- (>>=) : WithOutput Registers -> (Registers -> WithOutput Registers) -> WithOutput Registers
-- Initializer: MkOutput "the output" CurrentState -> WithOutput StateType
data WithOutput : (state : Type) -> Type where
    MkOutput : String -> state -> WithOutput state

NoOutput : {state : Type} -> state -> WithOutput state
NoOutput = MkOutput ""

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
data EvenVect : Nat -> Type -> Type where
    Empty : {n : Nat = Z} -> {ty : Type} -> EvenVect 0 ty
    FirstTwo : {k : Nat} -> {ty : Type} -> ty -> ty -> Vect (k + k) ty -> EvenVect (S k) ty

Foldable (EvenVect n) where
    foldr f e Empty = e
    foldr f e (FirstTwo a b xs) = foldr f e (a::b::xs)

-- lemma : (left : Nat) -> (right : Nat) -> Vect (S (S (left + right))) (Fin 8) = Vect (S (left + S right)) (Fin 8)
-- lemma left right = rewrite plusSuccRightSucc left right in Refl

lemma1 : (k : Nat) -> k + S k = S (k + k)
lemma1 k = rewrite plusSuccRightSucc k k in Refl

lemma2 : (k : Nat) -> S k + S k = S ((S k) + k)
lemma2 k = rewrite plusSuccRightSucc k k in Refl

lemma3 : (k : Nat) -> S k + S k = S (k + (S k))
lemma3 k = rewrite lemma2 k in Refl

parseCombo : Registers -> Fin 8 -> Int
parseCombo (MkRegisters a b c ip) 7 = 0
parseCombo (MkRegisters a b c ip) 6 = c
parseCombo (MkRegisters a b c ip) 5 = b
parseCombo (MkRegisters a b c ip) 4 = a
parseCombo (MkRegisters a b c ip) k = cast (finToNat k)

run : {n : Nat} -> Registers -> EvenVect n (Fin 8) -> WithOutput Registers
run r Empty = MkOutput "" r
-- adv (division a)
-- this is a bitshift right: "The numerator is the value in the A register. The denominator is found by raising 2 to the power of the instruction's combo operand."
run r@(MkRegisters a b c ip) (FirstTwo 0 combo xs) = NoOutput (MkRegisters (a `shiftR` (restrict 63 (cast (parseCombo r combo)))) b c (ip + 2))
-- bxl (bitwise or)
run r@(MkRegisters a b c ip) (FirstTwo 1 literal xs) = NoOutput (MkRegisters a (b `xor` (cast (finToInteger literal))) c (ip + 2))
-- bst (mod 8)
run r@(MkRegisters a b c ip) (FirstTwo 2 combo xs) = ?hole2
-- jnz (jump if nonzero)
run r@(MkRegisters a b c ip) (FirstTwo 3 literal xs) = ?hole3
-- bxc (bitwise or)
run r@(MkRegisters a b c ip) (FirstTwo 4 _ xs) = ?hole4
-- out
run r@(MkRegisters a b c ip) (FirstTwo 5 combo xs) = ?hole5
-- bdv (division b)
run r@(MkRegisters a b c ip) (FirstTwo 6 combo xs) = ?hole6
-- cdv (division c)
run r@(MkRegisters a b c ip) (FirstTwo 7 combo xs) = ?hole7
run r@(MkRegisters _ _ _ _) (FirstTwo _ _ _) = ?run_missing_case_1 -- should not need to exist?? we've covered all of the 8 cases

part1 : String -> Int
part1 input = 1

-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2