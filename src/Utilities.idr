module Utilities

import System.File
import Data.Either
import Data.Maybe
import Data.String
import Data.Fin
import Control.Monad.ST
import Data.SortedMap

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
    solve : Fin 2 -> String -> Int

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
memoize' : (Ord a) => (a -> b) -> a -> ST s b
memoize' f input = do
    memoRef <- newSTRef (Data.SortedMap.empty)
    memo <- readSTRef memoRef
    case lookup input memo of
        Just output => pure output
        Nothing => do
            let output = f input
            modifySTRef memoRef (insert input output)
            pure output

public export
memoize : (Ord a) => (a -> b) -> (a -> b)
memoize f input = runST (memoize' f input)