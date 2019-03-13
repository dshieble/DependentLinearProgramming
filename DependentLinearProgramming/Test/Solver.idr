{--
idris --testpkg DependentSimplex.ipkg
--}

module DependentLinearProgramming.Test.Solver

import DependentLinearProgramming.Core
import DependentLinearProgramming.Utilities
import DependentLinearProgramming.Solver

import Test.Unit
import Test.Unit.Assertions
import Data.Vect
import Data.Matrix
import Data.Matrix.Numeric
import Text.PrettyPrint.WL

%access export


prog1 : Program 3 3
prog1 = MkProgram [5,4,3] [[2,3,1], [4,1,2], [3,4,2]] [5,11,8]
solution1 : Solution 3
solution1 = solveLinearProgram prog1 {cm=2} {v=3}
test1a : IO Bool
test1a = assertTrue ((value solution1) == 13)
test1b : IO Bool
test1b = assertTrue ((weights solution1) == [2,0,1])


prog2 : Program 3 3
prog2 = MkProgram [3,2,-4] [[1,4,0], [2,4,-2], [1,1,-2]] [5,6,2]
solution2 : Solution 3
solution2 = solveLinearProgram prog2 {cm=2} {v=3}
test2a : IO Bool
test2a = assertTrue ((value solution2) == 8)
test2b : IO Bool
test2b = assertTrue ((weights solution2) == [4,0,1])

prog3 : Program 3 2
prog3 = MkProgram [80, 70] [[6,3], [1,1], [2,6]] [96,18,72]
solution3 : Solution 2
solution3 = solveLinearProgram prog3 {cm=2} {v=2}
test3a : IO Bool
test3a = assertTrue ((value solution3) == 1400)
test3b : IO Bool
test3b = assertTrue ((weights solution3) == [14, 4])

prog4 : Program 1 5
prog4 = MkProgram [1,-5,10,-2,15] [[1,1,1,1,1]] [100]
solution4 : Solution 5
solution4 = solveLinearProgram prog4 {cm=0} {v=5}
test4a : IO Bool
test4a = assertTrue ((value solution4) == 1500)
test4b : IO Bool
test4b = assertTrue ((weights solution4) == [0, 0, 0, 0, 100])

prog5 : Program 1 1
prog5 = MkProgram [1] [[2]] [10]
solution5 : Solution 1
solution5 = solveLinearProgram prog5 {cm=0} {v=1}
test5a : IO Bool
test5a = assertTrue ((value solution5) == 5)
test5b : IO Bool
test5b = assertTrue ((weights solution5) == [5])

prog6 : Program 1 2
prog6 = MkProgram [2,-2] [[1,1]] [10]
solution6 : Solution 2
solution6 = solveLinearProgram prog6 {cm=0} {v=2}
test6a : IO Bool
test6a = assertTrue ((value solution6) == 20)
test6b : IO Bool
test6b = assertTrue ((weights solution6) == [10,0])


runTest : IO ()
runTest = do
  NonReporting.runTests [
    test1a
  , test1b
  , test2a
  , test2b
  , test3a
  , test3b
  , test4a
  , test4b
  , test5a
  , test5b
  , test6a
  , test6b
  ]
