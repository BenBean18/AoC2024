module Utilities

import System.File
import Data.Either
import Data.Maybe
import Data.String
import Data.Fin

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