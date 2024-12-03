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
solve : Fin 2 -> String -> Int
solve 0 = part1
solve 1 = part2