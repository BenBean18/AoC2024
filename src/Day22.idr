module Day22

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

-- let's just write this literally for now
-- i'm guessing that part 2 will require simulating many more than 2000 so number theory/some fancy modular arithmetic stuff/cycle detection
-- but premature optimization is the root of all evil, just have to try

-- mod 16777216 = 24 bit number

-- we can just store in 24-bit number to avoid having to "prune"

-- eh that's not standard

-- mix then prune is just xor then be 24-bit
-- also mix then prune is commutative (mixPrune x y = mixPrune y x)

mixPrune : Int -> Int -> Int
mixPrune = xor . (`mod` 16777216)

nextNumber : Int -> Int
nextNumber secret = 
    let functions : List (Int -> Int) = [(*) 64, (`div` 32), (*) 2048] in foldl (\secret, fn => mixPrune (fn secret) secret) secret functions

nthNumber : {n : Nat} -> Int -> Int
nthNumber {n=Z} secret = secret
nthNumber {n=(S k)} secret = nthNumber {n=k} (nextNumber secret)

partial part1 : String -> Int
part1 input = sum (map (nthNumber {n=2000}) (map cast (lines input)))

-- Part 2

price : Int -> Int
price = (`mod` 10)

-- uhhhhh
-- this seems like an optimization problem
-- find the optimal sequence of four
-- how many possible sequences of four are there
-- well each digit can be (-9..9)...so 19 possibilities
-- 19^4 = 130321 possible sequences

-- ok idea
-- iterate through all windows of 5 that appear in each of them
-- store the price that appears after each window in a SortedMap (Vect 4 Int) (List Int)
-- and then you just find the key corresponding to the highest value in the map

-- WOAH
-- i had this with lists before and had to add the empty case list
-- forcing Vect (S k) a caused that to be a type error since it can't exist :)
differences : Neg a => Vect (S k) a -> Vect k a
differences (x::xs) = zipWith (-) (tail (x::xs)) (init (x::xs))

nPrices : {n : Nat} -> Int -> List Int
nPrices {n=Z} i = []
nPrices {n=(S k)} i = price i :: nPrices {n=k} (nextNumber i)

-- more efficient to just store a vector of 5 at a time and fill it with nextNumber, but a list of 2000 might not be *terrible*
updateMap : SortedMap (Vect 4 Int) Int -> List Int -> SortedMap (Vect 4 Int) Int
updateMap m (p1::p2::p3::p4::price::xs) = updateMap (insertWith (\new, old => old) (differences [p1,p2,p3,p4,price]) price m) (p2::p3::p4::price::xs)
updateMap m _ = m

partial part2 : String -> Int
part2 input = 
    let maps = foldl (mergeWith (+)) empty ((map (\i => (updateMap empty (nPrices {n=2000} i))) (map cast (lines input))))
        l = values maps in (ne last) (sort l)

public export
partial solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2