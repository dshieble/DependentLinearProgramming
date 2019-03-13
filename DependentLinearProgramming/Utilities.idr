module DependentLinearProgramming.Utilities

import Data.Vect
import Data.Matrix
import Data.Matrix.Numeric

%access export


------------------------ Printing Helpers ----------------------------
total
printVector : Vect (S n) Double -> String
printVector v = foldr1 (++) (map (++ " ") (map prim__floatToStr v))

total
printMatrix : Matrix (S n) (S m) Double -> String
printMatrix mat = foldr1 (++) (map (++ " \n ") (map printVector mat))

total
printMaybe : Maybe a -> (a -> String) -> String
printMaybe maybeVal printfn = case maybeVal of
  Nothing => "NOTHING"
  Just mat => "Just \n" ++ (printfn mat)


------------------------ Matrix and Index Helpers ----------------------------
total
cath : (Matrix n j a) -> (Matrix n k a) -> (Matrix n (j+k) a)
cath m1 m2 = zipWith (\v1,v2 => (v1 ++ v2)) m1 m2

total
catv : (Matrix j n a) -> (Matrix k n a) -> (Matrix (j+k) n a)
catv m1 m2 = m1 ++ m2

total
mkZeros : (n : Nat) -> Vect n Double
mkZeros Z = Nil
mkZeros (S k) = 0 :: (mkZeros k)

total
Zeros : Vect n Double
Zeros = mkZeros _

total
mkTightFinite : (n: Nat) -> Fin (S n)
mkTightFinite Z = FZ
mkTightFinite (S k) = FS (mkTightFinite k)

total
finSubtractHelper : (maxval : Nat) -> (val: (Fin (S maxval))) -> Fin (S (minus maxval (finToNat val)))
finSubtractHelper maxval FZ = rewrite minusZeroRight maxval in mkTightFinite maxval
finSubtractHelper (S mx) (FS r) = finSubtractHelper mx r

total
finSubtract : (maxval : Nat) -> (val: (Fin maxval)) -> Nat
finSubtract mx r = finToNat (finSubtractHelper mx (weaken r))



------------------------ Proofs ----------------------------

total
solverRewrite1 : (cm : Nat) -> S (S cm) = (S cm) + 1
solverRewrite1 cm =  rewrite plusCommutative (S cm) 1 in Refl

total
solverRewrite2 : (v : Nat) -> (cm : Nat) -> (v + (S cm)) + 1 = S (S (v + cm))
solverRewrite2 v cm =
  rewrite sym $ plusSuccRightSucc v cm in
  rewrite plusCommutative (S (v + cm)) 1 in Refl

total
plus1EqSucc : (cm : Nat) -> (S (S cm)) = ((S cm) + 1)
plus1EqSucc Z = Refl
plus1EqSucc (S k) = rewrite plus1EqSucc k in Refl

-- TODO: Complete this proof
total
pivotRewrite : (scm : Nat) -> (rind : (Fin scm)) -> (S scm = (finToNat rind) + (S (finSubtract scm rind)))
pivotRewrite scm rind  = rewrite sym (assert_total (pivotRewrite scm rind)) in Refl
