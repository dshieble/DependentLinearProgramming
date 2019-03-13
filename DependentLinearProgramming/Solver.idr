{--
The simplex algorithm is as follows:
  Repeat:
    - Select pivot column with the most negative bottom element (last row). If none are negative, terminate.
    - Generate the "ratio vector" by dividing each element in the far-right column by the corresponding element in the pivot column
    - Pick the index of the ratio vector that corresponds to the smallest non-negative quotient. That is the index of the pivot row
    - Use the selected (row, column) element to zero-out all of the other elements in its column with Gaussian Elimination
--}

module DependentLinearProgramming.Solver

import DependentLinearProgramming.Core
import DependentLinearProgramming.Utilities

import Data.Vect
import Data.Matrix
import Data.Matrix.Numeric

%access export

public export record Program (c: Nat) (v: Nat) where
  constructor MkProgram
  primal : Vect v Double
  constraintWeights : Matrix c v Double
  constraintTargets : Vect c Double

public export record Solution (v: Nat) where
  constructor MkSolution
  weights : Vect v Double
  value : Double


total
makeAugmentedMatrix : {c: Nat} -> {v: Nat} -> Program c v -> Matrix (c + 1) (v + c + 1) Double
makeAugmentedMatrix prog = catv top bottom  where
  top = cath (cath (constraintWeights prog) Id) (col (constraintTargets prog))
  bottom = cath (cath (row (primal prog)) (row Zeros)) [[0]]


optimizeAugmentedMatrix : Matrix (S (S cm)) (S (S (v + cm))) Double -> Matrix (S (S cm)) (S (S (v + cm))) Double
optimizeAugmentedMatrix augMat = case getPivot augMat of
  Nothing => augMat
  Just (r, c) => optimizeAugmentedMatrix ((mkPivotMatrix augMat (r,c)) <> augMat)


getSolutionFromAugmentedMatrix : Matrix (S (S cm)) (S (S (v + cm))) Double -> Solution v
getSolutionFromAugmentedMatrix {cm} {v} mat  = MkSolution (map (\ix => getValue mat (weakenN (S (S cm)) ix)) range) (-last (last mat)) where
  findOne : Vect (S (S cm)) Double -> Maybe (Fin (S (S cm)))
  findOne vec = fst (foldl (func vec) (Nothing, True) (zip range vec)) where
    func vec (maybeInd, searching) (ix, el) =
        if (searching && (el == 1)) then (Just ix, False)
        else if (el == 0) then (maybeInd, searching)
        else (Nothing, False)
  getValue : Vect (S (S cm)) (Vect (S (S (plus v cm))) Double) -> Fin (plus v (S (S cm))) -> Double
  getValue smat colIndex =
    let
      reColIndex : Fin (S (S (v + cm))) =
          rewrite plusSuccRightSucc v cm in
          rewrite plusSuccRightSucc v (S cm) in colIndex
    in
      case findOne (map (index reColIndex) smat) of
        Nothing => 0
        Just oneInd => last (index oneInd smat)


-- Solve a linear program with at least one constraint
solveLinearProgram : Program (S cm) v -> Solution v
solveLinearProgram {cm} {v} prog =
  let
    rewrittenAug : Matrix (S (S cm)) (S (S (v + cm))) Double =
        rewrite solverRewrite1 cm in
        rewrite sym $ solverRewrite2 v cm in (makeAugmentedMatrix prog)
    optimizedAug : Matrix (S (S cm)) (S (S (v + cm))) Double = optimizeAugmentedMatrix rewrittenAug {cm=cm} {v=v}
  in
    getSolutionFromAugmentedMatrix optimizedAug {cm=cm} {v=v}
