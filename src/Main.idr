module Main

-- https://juliu.is/peeling-zeroes/

import System
import Utilities
import Data.Maybe
import Data.Fin
import Data.Nat
import Data.List
import System.Clock
import Debug.Trace
import Day5_23
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13

-- I was trying to do this with Data.List.index but haven't figured out proofs yet
-- https://stackoverflow.com/questions/48995850/proving-an-index-is-within-list-bounds-given-index-1-is-within-bounds was what I was looking at
-- gave up and wrote my own index function
index : List a -> Nat -> Maybe a
index (x :: xs) Z = Just x
index (x :: xs) (S k) = index xs k
index [] _ = Nothing

runMultipleTimes : Nat -> IO a -> IO (List a)
runMultipleTimes Z f = pure []
runMultipleTimes (S k) f = do
    a <- f
    b <- runMultipleTimes k f
    pure (a :: b)

bench' : (String -> IO Int) -> String -> IO Integer
bench' f input = do
    start <- clockTime Process
    a <- f input
    end <- clockTime Process
    pure ((seconds (timeDifference end start) * 1_000_000) + (nanoseconds (timeDifference end start) `div` 1_000))

bench : (String -> IO Int) -> String -> IO ()
bench f input = do
    runtimes <- runMultipleTimes 100 (bench' f input)
    putStr $ show (sum runtimes `div` 100) ++ "us"

runPart : (String -> IO Int) -> String -> IO ()
runPart f input = do
    soln <- f input
    putStr $ show soln

main : IO ()
main = do
    args <- getArgs
    if (length args >= 3) then
        do
            let day = fromMaybe "" (args `index` 1)
            let partStr = fromMaybe "" (args `index` 2)
            let doBench = fromMaybe "" (args `index` 3) == "b"
            case integerToFin (cast partStr - 1) 2 of
                Just part => do
                    let message = (if doBench then "Benchmarking" else "Executing") ++ " Day " ++ day ++ " Part " ++ partStr
                    putStrLn message
                    contents <- getString $ "input/" ++ day ++ ".txt"
                    let run = if doBench then bench else runPart
                    if day == "5_23" then run (Day5_23.solve part) contents
                        else if day == "1" then run (Day1.solve part) contents
                        else if day == "2" then run (Day2.solve part) contents
                        else if day == "3" then run (Day3.solve part) contents
                        else if day == "4" then run (Day4.solve part) contents
                        else if day == "5" then run (Day5.solve part) contents
                        else if day == "6" then run (Day6.solve part) contents
                        else if day == "7" then run (Day7.solve part) contents
                        else if day == "8" then run (Day8.solve part) contents
                        else if day == "9" then run (Day9.solve part) contents
                        else if day == "10" then run (Day10.solve part) contents
                        else if day == "11" then run (Day11.solve part) contents
                        else if day == "12" then run (Day12.solve part) contents
                        else if day == "13" then run (Day13.solve part) contents
                        else putStr "That problem doesn't exist (or I haven't solved it yet)"
                    putStrLn ""
                Nothing => putStrLn $ "Part " ++ partStr ++ " is invalid"
        
        else putStrLn "Provide more arguments"