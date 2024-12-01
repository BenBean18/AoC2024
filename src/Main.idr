module Main

-- https://juliu.is/peeling-zeroes/

import System
import Utilities
import Data.Maybe
import Data.Fin
import Data.Nat
import Data.List
import Day5_23
import Day1

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
            let partStr = fromMaybe "" (args `index` 2)
            case integerToFin (cast partStr - 1) 2 of
                Just part => do
                    let message = "Executing Day " ++ day ++ " Part " ++ partStr
                    putStrLn message
                    contents <- getString $ "input/" ++ day ++ ".txt"
                    if day == "5_23" then print (Day5_23.solve part contents)
                        else if day == "1" then print (Day1.solve part contents)
                        else putStr "That problem doesn't exist (or I haven't solved it yet)"
                    putStrLn ""
                Nothing => putStrLn $ "Part " ++ partStr ++ " is invalid"
        
        else putStrLn "Provide more arguments"