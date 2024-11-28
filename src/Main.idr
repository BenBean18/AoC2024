module Main

-- https://juliu.is/peeling-zeroes/

import System
import Utilities
import Data.List
import Data.Maybe
import Data.Fin
import Data.Nat

-- https://stackoverflow.com/questions/48995850/proving-an-index-is-within-list-bounds-given-index-1-is-within-bounds
inBoundsDecr : InBounds (S k) xs -> InBounds k xs
inBoundsDecr {k=Z} (InLater y) = InFirst
inBoundsDecr {k=(S k)} (InLater y) = InLater (inBoundsDecr y)

-- https://github.com/orez-/Advent-of-Code/blob/dc9d44ded00409fe8ce6a8b93b1ab0dcf1d63b6f/2021/day09/part1.idr might help

main : IO ()
main = do
    args <- getArgs
    -- case inBounds 2 args of -- thank goodness for github search, i was going insane https://github.com/search?q=inBounds+language%3AIdris&type=code
    --     No _ => assert_total $ idris_crash "Not enough CLI arguments!"
    --     Yes _ => do
    --         -- let day = index @{prf} 1 args
    --         let day = index 1 args
    --         let part = index 2 args
    --         -- let message = "Executing Day " ++ [fromMaybe "" day] ++ " Part " ++ [fromMaybe "" part]
    --         -- print message
    --         print "hi"
    if (length args >= 2) then print (index 2 args) else print ""