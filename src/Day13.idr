module Day13

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Data.Nat
import Utilities
import Data.SortedMap
import Data.SortedSet
import Data.Vect
import Data.List1

-- it costs 3 tokens to push the A button and 1 token to push the B button.
-- You wonder: what is the smallest number of tokens you would have to spend to win as many prizes as possible?

{-
Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400
 -}

-- A 80, B 40 is the sol'n for this

-- You estimate that each button would need to be pressed no more than 100 times to win a prize. How else would someone be expected to play?

-- A is more expensive than B

-- Brute force method: 0 B, 0 A - ... - 100 B, 0 A - 0 B, 1 A - ... - 100 B, 1 A - ... - 100 B, 100 A

{-

b_a = [94; 34]
b_b = [22; 67]
A = [94 22;
     34 67];

b = [8400; 5400];

Solve Ax = b
 -}

-- Finding all linear combinations of A and B such that (1B + 3A) is minimized

-- Is there any way for there to be multiple solutions? Yes, if there is a solution such that Ax = 0, or A is singular
-- If A is singular, we run into a slight problem
-- We can definitely bruteforce for nonsingular matrices

-- 2x2 inverse is closed-form

Mat: Nat -> Type -> Type
Mat n a = Vect n (Vect n a)

inv2 : Mat 2 Double -> Maybe (Mat 2 Double)
inv2 ((a :: b :: []) :: (c :: d :: []) :: []) =
    let det' = a*d - b*c in
        case det' of
            0 => (trace "singular") Nothing -- singular!
            det => Just (((1 / det) * d :: (1 / det) * (-b) :: []) :: ((1 / det) * (-c) :: (1 / det) * a :: []) :: [])

-- -- https://stackoverflow.com/a/7922967
-- -- Euclidean algorithm
-- gcd : Int -> Int -> Int
-- gcd a 0 = a
-- gcd a b = gcd b (a `mod` b)

-- reduce : (Int,Int) -> (Int,Int)
-- reduce (0,0) = (0,0) -- no div by 0
-- reduce (a,b) = let g = gcd (abs a) (abs b) in ((a `div` g), (b `div` g))

parseNums : String -> List Int
parseNums s = map cast $ map pack $ filter (\x => length x > 0) $ map ((filter isDigit) . unpack) (words s)

parseMachine : List String -> Maybe (Mat 2 Double, Vect 2 Double)
parseMachine (a :: b :: goal :: []) =
    let aXY = parseNums a
        bXY = parseNums b
        goal : List Double = map cast (parseNums goal) in 
            case aXY of 
                (a :: c :: []) => 
                    case bXY of
                        (b :: d :: []) => 
                            let m : Mat 2 Double = ((cast a :: cast b :: []) :: (cast c :: cast d :: []) :: []) in 
                                case goal of 
                                    (x :: y :: []) => Just (m, (cast x :: cast y :: []))
                                    _ => Nothing
                        _ => Nothing
                _ => Nothing
parseMachine _ = Nothing

-- scalar multiplication:
scalarMultiply : (Num a) => a -> Vect n a -> Vect n a
scalarMultiply scalar l = map (* scalar) l

(n: Nat) => Num t => Num (Vect n t) where
    fromInteger i = replicate n 0 -- nothing

    (*) a b = replicate n 0 -- we only care about addition, this isn't defined

    (+) a b = let   pairs : Vect n (t,t) = zip a b
                    sums : Vect n t = map (\(i,j) => i + j) pairs in sums

-- the vec-mat: take a linear combination of the rows (can transpose matrix to do mat-vec)
vecMat : (Num a) => {n : Nat} -> Vect n a -> Mat n a -> Vect n a
vecMat vec mat =
    let scalarRowPairs : Vect n (a, Vect n a) = zip vec mat in sum (map (uncurry scalarMultiply) scalarRowPairs)

matVec : (Num a) => {n : Nat} -> Mat n a -> Vect n a -> Vect n a
matVec mat vec = vecMat vec (transpose mat)

round : Double -> Int
round d = cast (0.5 + d)

findSolution : (Mat 2 Double, Vect 2 Double) -> Maybe (Vect 2 Int)
findSolution (a, b) =
    let a_inv' = inv2 a in
        case a_inv' of
            Just a_inv =>
                let product : Vect 2 Double = matVec a_inv b
                    rounded : Vect 2 Int = map round product in
                        case (map round (a `matVec` (map cast rounded))) == map round b of
                            True => Just rounded
                            False => Nothing
            Nothing => Nothing

score : Maybe (Vect 2 Int) -> Int
score (Just (a :: b :: [])) = 3*a + b
score Nothing = 0

-- Part 1: 711us

part1 : String -> Int
part1 input = 
    let groups = forget $ splitOn "" (lines input)
        machines' = filter isJust (map parseMachine groups)
        machines = map (\a => fromJust @{believe_me (Just a)} a) machines' in sum (map (score . findSolution) machines)

-- Part 2: 700us

parseMachine2 : List String -> Maybe (Mat 2 Double, Vect 2 Double)
parseMachine2 (a :: b :: goal :: []) =
    let aXY = parseNums a
        bXY = parseNums b
        goal : List Double = map cast (parseNums goal) in 
            case aXY of 
                (a :: c :: []) => 
                    case bXY of
                        (b :: d :: []) => 
                            let m : Mat 2 Double = ((cast a :: cast b :: []) :: (cast c :: cast d :: []) :: []) in 
                                case goal of 
                                    (x :: y :: []) => Just (m, (10000000000000 + cast x :: 10000000000000 + cast y :: []))
                                    _ => Nothing
                        _ => Nothing
                _ => Nothing
parseMachine2 _ = Nothing

part2 : String -> Int
part2 input = 
    let groups = forget $ splitOn "" (lines input)
        machines' = filter isJust (map parseMachine2 groups)
        machines = map (\a => fromJust @{believe_me (Just a)} a) machines' in sum (map (score . findSolution) machines)

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2