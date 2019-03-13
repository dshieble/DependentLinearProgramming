{--
This module contains code for building the pivot matrix
--}

module DependentLinearProgramming.Core

import DependentLinearProgramming.Utilities

import Data.Vect
import Data.Matrix
import Data.Matrix.Numeric

%access export

-- This method accepts two numbers and returns True if the second is negative or the first is smaller than the second
total
compare: Double -> Double -> Bool
compare celem melem =
    if (celem < 0) then False
    else if (melem < 0) then True
    else (celem < melem)

total
getMinimumIndex : Vect (S l) Double -> Fin (S l)
getMinimumIndex v = fst (getFolded v) where
  accfunc : (Fin (S l), Double) -> (Fin (S l), Double) -> (Fin (S l), Double)
  accfunc (mind, melem) (cind, celem) = if (compare celem melem) then (cind,celem) else (mind,melem)
  getInitial : Vect (S l) Double -> (Fin (S l), Double)
  getInitial v = (FZ, (head v))
  getFolded : Vect (S l) Double -> (Fin (S l), Double)
  getFolded v = (foldl accfunc (getInitial v) (zip range v))


-- This method accepts an augmented matrix A and returns the row r of the pivot element
total
getPivotRow : Matrix (S (S cm)) (S (S (v + cm))) Double  -> Fin (S (S (v + cm))) -> Fin (S cm)
getPivotRow {cm} {v} mat col =
  let
    rewrittenMat : Matrix ((S cm) + 1) (S (S (v + cm))) Double = rewrite sym $ plus1EqSucc cm in mat
    takenMat = (take (S cm) rewrittenMat {m=1})
  in
    getMinimumIndex (map (\vec => (last vec) / (index col vec)) takenMat) {l=cm}


total
findNonNegativeIndex : Vect l Double -> Maybe (Fin l)
findNonNegativeIndex [] = Nothing
findNonNegativeIndex (a :: xs) = if a > 0 then (Just 0) else map FS (findNonNegativeIndex xs)

-- This method accepts an augmented matrix A and returns Nothing if A is optimal and otherwise returns Just c, where c is the column of the pivot element
total
getPivotCol : Matrix (S aa) bb Double -> Maybe (Fin bb)
getPivotCol mat = findNonNegativeIndex (last mat)


-- This method accepts an augmented matrix and returns Nothing if it's optimal, and a pivot element otherwise
total
getPivot : Matrix (S (S cm)) (S (S (v + cm))) Double -> (Maybe (Fin (S cm), Fin (S (S (v + cm)))))
getPivot mat = map (\col => ((getPivotRow mat col), col)) (getPivotCol mat)



total
mkLeft : Matrix nrows _ _ -> (rind : Nat) -> Matrix nrows rind Double
mkLeft _ rind = map replaceFunc range where
  replaceFunc : Fin nrows -> Vect rind Double
  replaceFunc ix = case natToFin (finToNat ix) rind of
    Nothing => (replicate rind 0)
    Just fIx => replaceAt fIx 1 (replicate rind 0)

total
mkRight : Matrix (S nrows) _ _ -> (rind : (Fin nrows)) -> Matrix (S nrows) (finSubtract nrows rind) Double
mkRight {nrows} _ rind = reverse (map (replaceFunc nrows rind) range) where
  replaceFunc nr r ix = case natToFin (finToNat ix) (finSubtract nr r) of
    Nothing => mkZeros _
    Just fIx => reverse (replaceAt fIx 1 (mkZeros _))


total
mkMiddle: Matrix (S nrows) (S ncols) Double -> (Fin (S nrows), Fin (S ncols)) -> Matrix (S nrows) 1 Double
mkMiddle mat (rind, cind) =
  let
    alpha = ((index cind (index rind mat)))
  in
    col (map (func alpha) range) where
      func alp ix = if (ix == rind) then (1/alp) else (-1/alp)*(index cind (index ix mat))


-- This method accepts an augmented matrix A and a pivot element (r,c) and returns the gaussian pivot matrix G about (r,c)
total
mkPivotMatrix : Matrix (S (S cm)) (S (S (v + cm))) Double -> (Fin (S cm), Fin (S (S (v + cm)))) -> Matrix (S (S cm)) (S (S cm)) Double
mkPivotMatrix {cm} {v} mat (rind, cind) =
  let
    leftRaw : Matrix (S (S cm)) (finToNat rind) Double =
      mkLeft mat (finToNat rind)
    left : Matrix ((finToNat rind) + (S (finSubtract (S cm) rind))) (finToNat rind) Double =
        rewrite sym $ pivotRewrite (S cm) rind in leftRaw
    rightRaw : Matrix (S (S cm)) (S (finSubtract (S cm) rind)) Double =
        cath (mkMiddle mat ((weaken rind), cind)) (mkRight mat rind)
    right : Matrix ((finToNat rind) + (S (finSubtract (S cm) rind))) (S (finSubtract (S cm) rind)) Double =
        rewrite sym $ pivotRewrite (S cm) rind in rightRaw

    out : Matrix ((finToNat rind) + (S (finSubtract (S cm) rind))) ((finToNat rind) + (S (finSubtract (S cm) rind))) Double =
      cath left right
  in
    rewrite pivotRewrite  (S cm) rind in out
