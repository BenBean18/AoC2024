module Utilities

import System.File
import Data.Either
import Data.Maybe
import Data.String
import Data.Fin
import Control.Monad.ST
import Data.SortedMap
import Debug.Trace

-- Note: `:search [signature]` in REPL is equivalent to Hoogle

-- Get text from file
-- Returns "" if error
public export
getString : String -> IO String
getString filename = (readFile filename) >>= (
    \output =>
        pure (fromMaybe "" (getRight output)))

-- Get list of ints from a string
public export
getInts : String -> List Int
getInts string = map cast (lines string)

-- Get bytes from a string
public export
getBytes : String -> List Bits8
getBytes string = map cast (unpack string)

public export
interface Solution where
    -- input: part and contents of input.txt, output: solution to print
    solve : Fin 2 -> String -> IO Int

public export
interface Visualization where
    -- input: contents of input.txt, output: solution to print, wrapped in an IO monad so we can do fancy visualization stuff
    visualize : String -> IO String

{-
Fun little backstory here!

I was working on some code for Graph Theory (trying to calculate the characteristic polynomial of the adjacency matrix of a path graph in Idris), and it was taking a REALLY long time for large polynomials.

I guessed this was because I was calculating a lot of factorials (within chooses), using my recursive factorial function that I implemented. To check this, I tried taking the factorial of larger numbers, and it was very slow.

I remembered that memoization is a way to solve this, but couldn't find a standard way to do this in Idris. I found the STRef mutable reference thingy (after some ChatGPT inspiration, but this is all my own code) and wrote this. I can now calculate polynomials quickly in that code, and have a tool that I know will be useful for future days :)
 -}

r : (Ord k) => ST s (STRef s (SortedMap k v))
r = newSTRef (Data.SortedMap.empty)

memoize' : (Show a) => (Ord a) => (a -> b) -> a -> ST s b
memoize' f input = do
    memoRef <- r
    memo <- readSTRef memoRef
    (trace $ show (length $ Data.SortedMap.toList memo)) $ case lookup input memo of
        Just output => (trace "found") pure output
        Nothing => do
            let output = f input
            (trace $ "inserted " ++ show input) $ modifySTRef memoRef (insert input output)
            pure output

public export
memoize : (Show a) => (Ord a) => (a -> b) -> (a -> b)
memoize f input = runST (memoize' f input)

mapIndexed : ((Nat, a) -> b) -> List a -> List b
mapIndexed f l = map f (zip [0..(length l `minus` 1)] l)

parseString' : Nat -> List (List Char) -> SortedMap (Int, Int) Char
parseString' rowIdx (row :: rows) = foldl
    (\m, ((r, c), e) => insert (r, c) e m) -- acc -> elem -> acc
    (parseString' (S rowIdx) rows) -- acc
    (mapIndexed (\(idx, el) => ((((the (Integer -> Int) cast) . natToInteger) rowIdx, ((the (Integer -> Int) cast) . natToInteger) idx), el)) row) -- List elem
parseString' _ [] = empty

public export
(+) : (Int, Int) -> (Int, Int) -> (Int, Int)
(+) (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

public export
(-) : (Int, Int) -> (Int, Int) -> (Int, Int)
(-) (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

public export
(*) : (Int, Int) -> (Int, Int) -> (Int, Int)
(*) (a1, b1) (a2, b2) = (a1 * a2, b1 * b2)

public export
div : (Int, Int) -> (Int, Int) -> (Int, Int)
div (a1, b1) (a2, b2) = (a1 `div` a2, b1 `div` b2)

public export
twoDStringToMap : String -> SortedMap (Int, Int) Char
twoDStringToMap l = parseString' Z (map unpack (lines l))

public export
ne : ((l: List a) -> {auto 0 _ : NonEmpty l} -> b) -> List a -> b
ne f l = f @{believe_me (NonEmpty l)} l

-- Mainly a template to modify for visualizations
public export
render2DMap : SortedMap (Int, Int) Char -> String
render2DMap m =
    let s = sort (keys m)
        (maxY, maxX) = ne last s
        (minY, minX) = ne head s
        toRender = map (\y => pack $ map (\x => (fromMaybe ' ') (lookup (cast y,cast x) m)) [minX..maxX]) [minY..maxY] in unlines toRender

public export
vis : Bool -> Lazy String -> a -> a
vis True s a = trace s a
vis False s a = a