module Template

import Data.String
import Data.List
import Debug.Trace
import Data.Fin
import Utilities

-- Part 1

part1 : String -> Int
part1 input = 1

-- Part 2

part2 : String -> Int
part2 input = 2

public export
solve : Fin 2 -> String -> IO Int
solve 0 = pure . part1
solve 1 = pure . part2