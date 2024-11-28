module Utilities

import System.File
import Data.Either
import Data.Maybe
import Data.String

-- Note: `:search [signature]` in REPL is equivalent to Hoogle

-- Get text from file
-- Returns "" if error
getString : String -> IO String
getString filename = (readFile filename) >>= (
    \output =>
        pure (fromMaybe "" (getRight output)))

-- Get list of lines from a file
getLines : String -> IO (List String)
getLines filename = (getString filename) >>= (\string => pure (lines string))

-- Get list of ints from a file
getInts : String -> IO (List Int)
getInts filename = (getLines filename) >>= (\lines => pure (map cast lines))

-- Get bytes from a file (why? idk)
getBytes : String -> IO (List Bits8)
getBytes filename = (getString filename) >>= (\string => pure (map cast (unpack string)))

main : IO ()
main = do
    s <- getString "input.txt"
    l <- getLines "input.txt"
    i <- getInts "input.txt"
    b <- getBytes "input.txt"
    putStrLn s
    print l
    print i
    print b