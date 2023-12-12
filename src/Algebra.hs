module Algebra where

import Model


-- Exercise 5
type Algebra r = ([Rule] -> r)

fold :: Algebra r -> Program -> r
fold (program) (Program rs) = program rs;



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined
