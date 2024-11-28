module Main

-- https://juliu.is/peeling-zeroes/

import System
import Utilities
import Data.Maybe
import Data.Fin
import Data.Nat
import Data.List
import Day5_23

-- I was trying to do this with Data.List.index but haven't figured out proofs yet
-- https://stackoverflow.com/questions/48995850/proving-an-index-is-within-list-bounds-given-index-1-is-within-bounds was what I was looking at
-- gave up and wrote my own index function
index : List a -> Nat -> Maybe a
index (x :: xs) Z = Just x
index (x :: xs) (S k) = index xs k
index [] _ = Nothing

main : IO ()
main = do
    args <- getArgs
    if (length args >= 3) then
        do
            let day = fromMaybe "" (args `index` 1)
            let part = fromMaybe "" (args `index` 2)
            let message = "Executing Day " ++ day ++ " Part " ++ part
            putStrLn message
            contents <- getString "src/input.txt"
            print (Day5_23.part1 contents)
            putStrLn ""
        
        else putStrLn "Provide more arguments"