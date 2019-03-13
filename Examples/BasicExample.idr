{--
idris -p dependent_linear_programming -p contrib Examples/BasicExample.idr -o BasicExample; ./BasicExample
--}
module Main
import DependentLinearProgramming.Solver
import DependentLinearProgramming.Utilities

import Data.Vect
import Data.Matrix
import Data.Matrix.Numeric
import Text.PrettyPrint.WL

program : Program 3 3
program = MkProgram [5,4,3] [[2,3,1], [4,1,2], [3,4,2]] [5,11,8]

solution : Solution 3
solution = solveLinearProgram program {cm=2} {v=3}

main : IO ()
main = do
  putStrLn ("Solution Weights: " ++ printVector (weights solution))
  putStrLn ("Solution Value: " ++ prim__floatToStr (value solution))
