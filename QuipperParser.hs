{-# LANGUAGE TupleSections #-}

module QuipperParser where

--import Bmv5
--import GateStruct

import Control.Monad
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified GateStruct as MyGateS
import Quipper.Internal.Circuit
import Quipper.Internal.Monad
import Quipper.Internal.Printing
import Quipper.Libraries.QuipperASCIIParser
import Quipper.Utils.Auxiliary
import System.Environment

test_gate = QGate "TT" False [0, 1, 2] [3, 4] [Signed 5 True, Signed 6 False] True

t_arity_in = IM.fromList (map (,Qbit) [0 .. 10])

t_arity_out = IM.fromList (map (,Qbit) [0 .. 5])

cir = (t_arity_in, [test_gate], t_arity_out, 11 :: Int)

bcir = (cir, M.empty)

exta = xintmap_inserts (IM.toList t_arity_in) xintmap_empty

quipper_gate2my_gate :: Gate -> MyGateS.Gate
quipper_gate2my_gate (QGate "not" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> MyGateS.X (head w1)
    1 -> if get_sign (head ctrls) then MyGateS.CX (head w1) (from_signed (head ctrls)) else MyGateS.Cnot' (head w1) (from_signed (head ctrls))
    2 -> case all get_sign ctrls of
      True -> MyGateS.Toffoli (head w1) (head (map from_signed ctrls)) (last (map from_signed ctrls))
    _ -> case all get_sign ctrls of
      True -> MyGateS.Toffolin (head w1 : map from_signed ctrls)
quipper_gate2my_gate (QGate "Z" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> MyGateS.Z (head w1)
    1 -> case get_sign (head ctrls) of
      True -> MyGateS.CZ (head w1) (from_signed (head ctrls))
    2 -> case all get_sign ctrls of
      True -> MyGateS.CCZ (head w1) (head (map from_signed ctrls)) (last (map from_signed ctrls))
    _ -> case all get_sign ctrls of
      True -> MyGateS.CNZ (head w1 : map from_signed ctrls)
quipper_gate2my_gate (QGate "H" False w1 w2 ctrls True) =
  case length ctrls of
    0 -> MyGateS.H (head w1)

parse :: String -> IO [MyGateS.Gate]
parse file = do
  str <- readFile file
  let (eps, circ) = parse_circuit str
  let len = length eps
  let exta1 = xintmap_inserts (map (,Qbit) [0 .. len -1]) xintmap_empty
  let (bcir@(c, n), a) = extract_simple id exta1 (circ eps)
  let (a1, gl, a2, mm) = c
  preview_bcircuit bcir
  print gl
  let gl' = map quipper_gate2my_gate gl
  --  topdf gl'
  return gl'

parseQuipper :: String -> IO (Int, [MyGateS.Gate])
parseQuipper str = do
  let (eps, circ) = parse_circuit str
  let len = length eps
  let exta1 = xintmap_inserts (map (,Qbit) [0 .. len -1]) xintmap_empty
  let (bcir@(c, n), a) = extract_simple id exta1 (circ eps)
  let (a1, gl, a2, mm) = c
  --  preview_bcircuit bcir
  --  putStrLn $ show gl
  let gl' = map quipper_gate2my_gate gl
  --  topdf gl'
  return (mm, gl')

parseQuipper' :: String -> IO [MyGateS.Gate]
parseQuipper' s = do
  str <- readFile s
  let (eps, circ) = parse_circuit str
  let len = length eps
  let exta1 = xintmap_inserts (map (,Qbit) [0 .. len -1]) xintmap_empty
  let (bcir@(c, n), a) = extract_simple id exta1 (circ eps)
  let (a1, gl, a2, mm) = c
  --  preview_bcircuit bcir
  --  putStrLn $ show gl
  let gl' = map quipper_gate2my_gate gl
  --  topdf gl'
  return gl'
