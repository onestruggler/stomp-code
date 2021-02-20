module GateStruct where

--import QuipperLib
--import QuipperLib.Qureg

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import Quipper
import Quipper.Internal.Monad
import Quipper.Internal.Printing
import System.Environment

--import Control.Applicative

-- | My paser for quantum circuit
data QState = Q0 | Q1 | QP | QM | QY | QMY deriving (Show, Eq, Ord, Read)

-- | Gate structrue
data Gate
  = I Int
  | Init QState Int -- for recording which wire is in state |0>
  | Init' String Int -- added for .qc parser
  | H Int
  | X Int
  | Y Int
  | Cnot Int Int
  | CX Int Int
  | CCX Int Int Int
  | Cnot' Int Int -- When control qubit is |0>, it fires
  | T Int
  | --            | T' Int
    CS Int Int
  | CS' Int Int
  | CZ Int Int
  | Swap Int Int
  | CCZ Int Int Int
  | Ga Int (Set.Set Int)
  | S Int
  | --            | S' Int
    Z Int
  | CNZ [Int] -- Added for the new translation
  | Toffoli Int Int Int -- standard toffoli
  | Toffoli0 Int Int Int -- fires when controls is |00>
  | Toffoli1 Int Int Int -- fires when controls is |01>
  | Toffoli2 Int Int Int -- fires when controls is |10>
  | Toffoli3 Int Int Int -- fires when controls is |11>
  | Toffoli4 Int Int Int Int
  | Toffolin [Int]
  | P [Int]
  | M Int
  | Term QState Int -- after Term0 i, the wire i is in state |0>
  | Term' String Int -- added for .qc parser
  | BDot -- added for matrix representation, black control
  | WDot -- added for matrix representation, white control
  | Nil -- added for matrix representation
  deriving (Eq, Ord, Read, Show)

isInitg :: Gate -> Bool
isInitg (Init _ _) = True
isInitg (Init' _ _) = True
isInitg x = False

{-
instance Show Gate where
  show Nil = " "
  show (H i) = "H"
  show (X i) = "X"
  show (Z i) = "•"
  show (S i) = "S"
  show BDot = "•"
  show (Toffoli i j k) = "T"
  show (Toffolin ins) = "T"
  show (CNZ ins) = "CN"
  show (CZ i j) = "CZ"
  show (CCZ i j k) = "CC"
  show (Cnot i j) = "C"
-}

reindexGate :: (Int -> Int) -> Gate -> Gate
reindexGate f (I i) = I (f i)
reindexGate f (X i) = X (f i)
reindexGate f (Y i) = Y (f i)
reindexGate f (H i) = H (f i)
reindexGate f (Cnot i j) = Cnot (f i) (f j)
reindexGate f (CX i j) = CX (f i) (f j)
reindexGate f (T i) = T (f i)
--reindexGate f (T' i) = T' (f i)
reindexGate f (CS i j) = CS (f i) (f j)
reindexGate f (CS' i j) = CS' (f i) (f j)
reindexGate f (CZ i j) = CZ (f i) (f j)
reindexGate f (Swap i j) = Swap (f i) (f j)
reindexGate f (CCZ i j k) = CCZ (f i) (f j) (f k)
reindexGate f (CNZ inds) = CNZ (map f inds)
reindexGate f (Ga i inds) = Ga i (Set.map f inds)
reindexGate f (S i) = S (f i)
--reindexGate f (S' i) = S' (f i)
reindexGate f (Z i) = Z (f i)
reindexGate f (Toffoli i j k) = Toffoli (f i) (f j) (f k)
reindexGate f (CCX i j k) = CCX (f i) (f j) (f k)
reindexGate f (Toffoli4 i j k l) = Toffoli4 (f i) (f j) (f k) (f l)
reindexGate f (Toffolin inds) = Toffolin $ map f inds
reindexGate f (P inds) = P $ map f inds
reindexGate f (M i) = M (f i)
reindexGate f (Init s i) = Init s (f i)
reindexGate f (Term s i) = Term s (f i)

reindexCir :: (Int -> Int) -> [Gate] -> [Gate]
reindexCir f = map (reindexGate f)

wiresOfGate :: Gate -> [Int]
wiresOfGate (I i) = [i]
wiresOfGate (X i) = [i]
wiresOfGate (Y i) = [i]
wiresOfGate (H i) = [i]
wiresOfGate (Cnot i j) = [i, j]
wiresOfGate (CX i j) = [i, j]
wiresOfGate (CCX i j k) = [i, j, k]
wiresOfGate (T i) = [i]
--wiresOfGate (T' i) = [i]
wiresOfGate (CS i j) = [i, j]
wiresOfGate (CS' i j) = [i, j]
wiresOfGate (CZ i j) = [i, j]
wiresOfGate (Swap i j) = [i, j]
wiresOfGate (CCZ i j k) = [i, j, k]
wiresOfGate (Ga i ws) = Set.toList ws
wiresOfGate (CNZ inds) = inds
wiresOfGate (S i) = [i]
--wiresOfGate (S' i) = [i]
wiresOfGate (Z i) = [i]
wiresOfGate (Toffoli i j k) = [i, j, k]
wiresOfGate (Toffoli4 i j k l) = [i, j, k, l]
wiresOfGate (Toffolin inds) = inds
wiresOfGate (P inds) = inds
wiresOfGate (M i) = [i]
wiresOfGate (Init s i) = [i]
wiresOfGate (Term s i) = [i]

wiresOfCir :: [Gate] -> [Int]
wiresOfCir term = foldl union [] (map wiresOfGate term)

--gate_at_ind :: (Qubit -> Circ ()) -> Int -> Circ ()
--gate_at_ind gate ind =

gateAB :: Gate -> (Qubit -> Circ ())
gateAB (I i) q = do
  let igate = named_gate_at "I"
  igate q
  return ()

gl2cir :: [Qubit] -> Gate -> Circ [Qubit]
gl2cir qs (I i) = return qs
gl2cir qs (Init s i) = do
  named_gate (decodeQState s) (qs !! (i -1))
  return qs
gl2cir qs (Term s i) = do
  named_gate (decodeQState' s) (qs !! (i -1))
  return qs
gl2cir qs (X i) = do
  gate_X (qs !! (i -1))
  return qs
gl2cir qs (Y i) = do
  gate_Y (qs !! (i -1))
  return qs
gl2cir qs (Z i) = do
  gate_Z (qs !! (i -1))
  return qs
gl2cir qs (S i) = do
  gate_S (qs !! (i -1))
  return qs
gl2cir qs (T i) = do
  gate_T (qs !! (i -1))
  return qs
--gl2cir qs (S' i) = do
--gate_S_inv (qs !! (i-1))
--return qs
--gl2cir qs (T' i) = do
--gate_T_inv (qs !! (i-1))
--return qs
gl2cir qs (H i) = do
  gate_H (qs !! (i -1))
  return qs
gl2cir qs (M i) = do
  label qs (replicate (length qs) "|")
  --  named_gate "M" (qs !! (i-1))
  return qs
gl2cir qs (CS' i j) = do
  gate_S_inv (qs !! (i -1)) `controlled` (qs !! (j -1))
  return qs
gl2cir qs (CS i j) = do
  gate_S (qs !! (i -1)) `controlled` (qs !! (j -1))
  return qs
gl2cir qs (Cnot i j) = do
  qnot (qs !! (i -1)) `controlled` (qs !! (j -1))
  return qs
gl2cir qs (CX i j) = do
  qnot (qs !! (i -1)) `controlled` (qs !! (j -1))
  return qs
gl2cir qs (CZ i j) = do
  gate_Z (qs !! (i -1)) `controlled` (qs !! (j -1))
  return qs
gl2cir qs (Swap i j) = do
  swap_at (qs !! (i -1)) (qs !! (j -1))
  return qs
gl2cir qs (CCZ i j k) = do
  gate_Z (qs !! (i -1)) `controlled` [qs !! (j -1), qs !! (k -1)]
  return qs
gl2cir qs (Ga k is) = do
  named_gate (show (k `mod` 8)) (qs !! (Set.findMin is -1)) `controlled` map (\x -> qs !! (x -1)) (Set.toList $ Set.delete (Set.findMin is) is)
  return qs
gl2cir qs (Toffoli i j k) = do
  qnot (qs !! (i -1)) `controlled` [qs !! (j -1), qs !! (k -1)]
  return qs
gl2cir qs (CCX i j k) = do
  qnot (qs !! (i -1)) `controlled` [qs !! (j -1), qs !! (k -1)]
  return qs
gl2cir qs (Toffoli4 i j k l) = do
  qnot (qs !! (i -1)) `controlled` [qs !! (j -1), qs !! (k -1), qs !! (l -1)]
  return qs
gl2cir qs (Toffolin is) = do
  qnot (qs !! (head is -1)) `controlled` map (\x -> qs !! (x -1)) (drop 1 is)
  return qs
gl2cir qs (P is) = do
  label (map (\x -> qs !! (x -1)) is) "P"
  return qs
gl2cir qs (CNZ is) = do
  gate_Z (qs !! (head is -1)) `controlled` map (\x -> qs !! (x -1)) (drop 1 is)
  return qs

-- | for translate {0,1,2...7} to {0, pi/4,....-pi/4}
decodeAngle :: Int -> String
decodeAngle n = case n `mod` 8 of
  0 -> "0"
  1 -> "pi/4"
  2 -> "pi/2"
  3 -> "3pi/4"
  4 -> "pi"
  5 -> "-3pi/4"
  6 -> "-pi/2"
  7 -> "-pi/4"

-- | for translate Qubit State {Q0,Q1,QP,QM} to {|0>, |1>, |+>, |->}
decodeQState :: QState -> String
decodeQState s = case s of
  Q0 -> "|0>"
  Q1 -> "|1>"
  QP -> "|+>"
  QM -> "|->"
  QY -> "|Y>"
  QMY -> "|-Y>"

-- | for translate Qubit State {Q0,Q1,QP,QM} to {|0>, |1>, |+>, |->}
decodeQState' :: QState -> String
decodeQState' s = case s of
  Q0 -> "<0|"
  Q1 -> "<1|"
  QP -> "<+|"
  QM -> "<-|"
  QY -> "<Y|"
  QMY -> "<-Y|"

gl2cir1 :: [Qubit] -> Gate -> Circ [Qubit]
gl2cir1 qs (I i) = return qs
gl2cir1 qs (X i) = do
  gate_X (qs !! i)
  return qs
gl2cir1 qs (Y i) = do
  gate_Y (qs !! i)
  return qs
gl2cir1 qs (Z i) = do
  gate_Z (qs !! i)
  return qs
gl2cir1 qs (S i) = do
  gate_S (qs !! i)
  return qs
gl2cir1 qs (T i) = do
  gate_T (qs !! i)
  return qs
--gl2cir1 qs (S' i) = do
--gate_S_inv (qs !! (i))
--return qs
--gl2cir1 qs (T' i) = do
--gate_T_inv (qs !! (i))
--return qs
gl2cir1 qs (H i) = do
  gate_H (qs !! i)
  return qs
gl2cir1 qs (M i) = do
  named_gate "M" (qs !! i)
  return qs
gl2cir1 qs (CS' i j) = do
  gate_S_inv (qs !! i) `controlled` (qs !! j)
  return qs
gl2cir1 qs (CS i j) = do
  gate_S (qs !! i) `controlled` (qs !! j)
  return qs
gl2cir1 qs (Cnot i j) = do
  qnot (qs !! i) `controlled` (qs !! j)
  return qs
gl2cir1 qs (CX i j) = do
  qnot (qs !! i) `controlled` (qs !! j)
  return qs
gl2cir1 qs (CZ i j) = do
  gate_Z (qs !! i) `controlled` (qs !! j)
  return qs
gl2cir1 qs (CCZ i j k) = do
  gate_Z (qs !! i) `controlled` [qs !! j, qs !! k]
  return qs
gl2cir1 qs (Toffoli i j k) = do
  qnot (qs !! i) `controlled` [qs !! j, qs !! k]
  return qs
gl2cir1 qs (Toffoli4 i j k l) = do
  qnot (qs !! i) `controlled` [qs !! j, qs !! k, qs !! l]
  return qs
gl2cir1 qs (Toffolin is) = do
  qnot (qs !! head is) `controlled` map (qs !!) (drop 1 is)
  return qs
gl2cir1 qs (P is) = do
  label (map (qs !!) is) "P"
  return qs

isT :: Gate -> Bool
isT (T i) = True
isT (CCZ i j k) = True
isT (Toffoli i j k) = True
isT (Ga i ws) = i `mod` 2 == 1
isT _ = False

test qs = do
  q <- qinit False
  p <- qinit True
  r <- qinit True
  gl2cir ([p, q, r] ++ replicate 10 qubit) (Cnot 1 2)
  gate_W_at q r
  gl2cir [p, q, r] (X 3)
  gl2cir [p, q, r] (P [1, 3])

main3 = do
  print_simple Preview test
