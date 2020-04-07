module Latex where

import GateStruct
import Circuit4b
import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import System.Environment
import System.IO
import Control.Monad.State

import Text.Printf
-- | Convert one gate to TikZ format. This returns a pair consisting
-- of: the width of the gate, and a function from an /X/-coordinate to
-- the code for the gate.
tikz_of_gate :: Int -> Gate -> (Double, Double -> [String])

tikz_of_gate n (T i) = (1.5, code) where
  code x = [printf "\\gate{$T$}{%0.2f,%d};" x i]
tikz_of_gate n (S i) = (1.5, code) where
  code x = [printf "\\gate{$S$}{%0.2f,%d};" x i]
tikz_of_gate n (Z i) = (1.5, code) where
  code x = [printf "\\gate{$Z$}{%0.2f,%d};" x i]

tikz_of_gate n (CZ i j) = (1, code) where
  code x = [printf "\\controlled{\\dotgate}{%0.2f,%d}{%d};" x i j]
tikz_of_gate n (CCZ i j k) = (1, code) where
  code x = [printf "\\controlled{\\dotgate}{%0.2f,%d}{%d,%d};" x i j k]

tikz_of_gate n (CX i j) = (1, code) where
  code x = [printf "\\controlled{\\notgate}{%0.2f,%d}{%d};" x i j]
tikz_of_gate n (Toffoli i j k) = (1, code) where
  code x = [printf "\\controlled{\\notgate}{%0.2f,%d}{%d,%d};" x i j k]

tikz_of_gate n (Ga p ws) = (1, code) where
  code x = [(printf "\\gencontrolled{\\gate{%d}}{\\gate{%d}}{%0.2f,%d}" p p x (ws'!!0)) ++ cws ++ ";"]
  ws' = Set.toList ws
  cws = string_of_list "{" "," "}" "{}" show ws'

tikz_of_gate n (H i) = (1.5, code) where
  code x = [printf "\\gate{$H$}{%0.2f,%d};" x i]
tikz_of_gate n (X i) = (1.5, code) where
  code x = [printf "\\gate{$X$}{%0.2f,%d};" x i]
tikz_of_gate n (Swap i j) = (1, code) where
  code x = [printf "\\swapgate{%0.2f}{%d}{%d};" x i j]

--tikz_of_gate n (Ga p ws) = (1, code) where
  --code x = [printf "\\swapgate{%0.2f}{%d}{%d};" x i j]


-- | Take a convex closure of a set of integers (represented as a
-- list).
convex :: (Enum a, Ord a) => [a] -> [a]
convex [] = []
convex l = [min..max] where
  min = minimum l
  max = maximum l

convex' l = [0..10]
-- | Assign columns to gates. The columns are numbered from 0. Gates
-- are sorted into columns in such a way that overlapping gates never
-- share the same column.
assign_columns :: [Gate] -> ([(Int, Gate)], Int)
assign_columns = aux Map.empty (-1) where
  aux :: Map Int Int -> Int -> [Gate] -> ([(Int, Gate)], Int)
  aux m c0 [] = ([], 1 + Map.foldr' max 0 m)
  aux m c0 (h:t) = (h':t', n) where
    (t', n) = aux m' c1 t
    h' = (c, h)
    wires = convex' (wiresOfGate h)
    c = 1 + foldl' (\x w -> max x (Map.findWithDefault (-1) w m)) c0 wires
    c1 = c0
    m' = foldr (\w m -> Map.insert w c m) m wires

-- | Print a Clifford circuit in TikZ format, with labels. The gates
-- are sorted into columns.
tikz_of_clifford_with_labels :: Int -> [Gate] -> [String] -> [String] -> String
tikz_of_clifford_with_labels n gates l1s l2s = str where
  (colgates, ncols) = assign_columns gates
  gatemap = foldr (\(c,g) m -> Map.insertWith (++) c [g] m) Map.empty colgates
  widthmap = Map.map (\gs -> maximum (map (fst . tikz_of_gate n) gs)) gatemap
  (width', centermap) = Map.mapAccum (\p w -> (p+w, p+0.5*w)) 0.5 widthmap
  render = do
    (c,g) <- colgates
    let ctr = centermap Map.! c
    let (_,f) = tikz_of_gate n g
    return (f ctr)
  width = width' + 0.5
  str = "\\begin{qcircuit}[scale=0.5]\n" ++
        printf "    \\grid{%0.2f}{%s}\n" width (string_of_list "" "," "" "" show [0..n-1]) ++
        concat [ "    " ++ r ++ "\n" | r <- leftlabels 0 n l1s ++
                                            concat render ++
                                            rightlabels width n l2s ] ++
        "\\end{qcircuit}\n"
  leftlabels :: Double -> Int -> [String] -> [String]
  leftlabels x n [] = []
  leftlabels x n (h:t) = (printf "\\leftlabel{%s}{%0.2f,%d}" h x (n-1)) : leftlabels x (n-1) t
  rightlabels :: Double -> Int -> [String] -> [String]
  rightlabels x n [] = []
  rightlabels x n (h:t) = (printf "\\rightlabel{%s}{%0.2f,%d}" h x (n-1)) : rightlabels x (n-1) t

-- | Print a Clifford circuit in TikZ format.
tikz_of_clifford :: Int -> [Gate] -> String
tikz_of_clifford n circ = tikz_of_clifford_with_labels n circ [] []


-- | A general list-to-string function. Example:
--
-- > string_of_list "{" ", " "}" "{}" show [1,2,3] = "{1, 2, 3}"
string_of_list :: String -> String -> String -> String -> (t -> String) -> [t] -> String
string_of_list lpar comma rpar nil string_of_elt lst =
  let string_of_tail lst =
        case lst of
          [] -> ""
          h:t -> comma ++ string_of_elt h ++ string_of_tail t
  in
   case lst of
     [] -> nil
     h:t -> lpar ++ string_of_elt h ++ string_of_tail t ++ rpar

-- | Print a list, one element per line.
print_list :: (Show a) => [a] -> IO ()
print_list as = sequence_ [ putStrLn (show a) | a <- as ]



------------------------------------------------------
-- | make each rewrite rules in moveh, moves, movecx concrete. This is
-- mainly for ouputing the rules in latex, and then print out the
-- rules in pdf.

type Rule = ([Gate], [Gate])
type Rules = [Rule]

hrules = map (\x -> (x, fst (mvhn 1 x) ++ snd (mvhn 1 x))) hrules_redexes where
  eval x = unJust $ execStateT (moveh x) ([],[])

hrule x = (x, fst (mvhn 10 x) ++ snd (mvhn 10 x))
hrule' x = (x, fst (mvs) ++ (snd $ snd (mvs))) where
  mvs = unJust $ runStateT (moveh_step x) ([],[])

srule x = (x, fst (mvs) ++ (snd $ snd (mvs))) where
  mvs = unJust $ runStateT (moves_step x) ([],[])

cxrule x = (x, fst (mvs) ++ (snd $ snd (mvs))) where
  mvs = unJust $ runStateT (movecx_step x) ([],[])

sfl = Set.fromList

cx_gad_rules = [
  ([CX 0 1, Ga 1 (sfl [3,4,2])],[Ga 1 (sfl [4,2,3]), CX 0 1]),
  ([CX 0 1, Ga 1 (sfl [3,1,2])],[Ga 1 (sfl [1,2,3]), CX 0 1]),  
--  ([CX 0 1, Ga 1 (sfl [0,3,2])],[Ga 1 (sfl [0,2,1,3]), CX 0 1]),
  ([CX 0 1, Ga 1 (sfl [0,1,2])],[Ga 1 (sfl [0,2]), CX 0 1]),
  ([Ga 1 (sfl[0,2,3]), Ga 2 (sfl [0,1,2])],[Ga 2 (sfl [0,2,1]), Ga 1 (sfl [0,2,3])]),
  ([Swap 0 1, Ga 1 (sfl [0,1,2])],[Ga 1 (sfl [0,1,2]), Swap 0 1]),
  ([Swap 0 1, Ga 1 (sfl [0,3,2])],[Ga 1 (sfl [1,3,2]), Swap 0 1])  
  ]


hrules_redexes = [
  [H 0, H 0],
  [H 0, T 0],  
  [H 0, T 1],
  [H 0, S 0, H 0],  
  [H 0, S 1],
  [H 0, Z 0],  
  [H 0, Z 1],
  [H 0, CZ 0 1],  
  [H 0, CCZ 1 2 3],  
  [H 0, X 0],
  [H 0, X 1],  
  [H 0, CX 0 1],
  [H 0, CX 1 0, H 0, T 0],
  [H 0, CX 1 0, H 1, T 1],    
  [H 0, CX 1 2],  
  [H 0, CX 1 0, S 0, H 0, T 0],
  [H 0, CX 1 0, S 1, H 1, T 1],  
  [H 0, CX 1 0, S 0, H 0],  
  [H 0, CX 1 0, S 1, H 1],  
  [H 0, CX 1 2],
  [H 0, CCZ 1 2 3],    
  [H 0, Swap 0 1]
  ]

srules_redexes = [
  [S 0, T 0],
  [S 0, S 0],
  [S 0, Z 0],
  [S 0, CZ 0 1],
  [S 0, CCZ 0 1 2],
  [S 0, X 0],
  [S 0, CX 0 1],
  [S 0, CX 1 0],
  [S 0, Swap 0 1]
  ]

cxrules_redexes = [
  [CX 1 0, Z 0],
  [CX 1 0, Z 1],  
  [CX 1 0, CZ 0 1],
  [CX 1 0, CZ 1 2],  
  [CX 1 0, CZ 0 2],
  [CX 1 0, X 1],  
  [CX 1 0, X 0],
  [CX 1 0, CX 1 0],  
  [CX 1 0, CX 0 1],
  [CX 1 0, CX 1 2],  
  [CX 1 0, CX 2 0],
  [CX 1 0, CX 2 1],  
  [CX 1 0, Swap 0 1],
  [CX 1 0, Swap 0 2]    
  ]


tikz_of_rules :: [Rule] -> String
tikz_of_rules rs = string_of_list "\\begin{eqnarray}\n" "\\\\\\nonumber\\\\[0ex]\n" "\\end{eqnarray}" "" tikz_of_rule rs

tikz_of_rule :: Rule -> String
tikz_of_rule (l,r) = s where
  s = "\\m{\n" ++ sl ++ "}&\\rightarrow &\n\\m{\n" ++ sr ++ "}\n"
  sl = tikz_of_clifford (length $ wiresOfCir l) l
  sr = tikz_of_clifford (length $ wiresOfCir r) r  

tikz_of_rule' :: Rule -> IO ()
tikz_of_rule' (l,r) = putStrLn s where
  s = "\\m{\n" ++ sl ++ "}&=&\n\\m{\n" ++ sr ++ "}"
  sl = tikz_of_clifford 0 l
  sr = tikz_of_clifford 0 r  


tikz_of_example :: Int -> [Gate] -> IO ()
tikz_of_example n cir = putStrLn s where
  nmv = map (\n -> mvhn n) [1..n]
  len = length $ wiresOfCir cir
  ns = map (\fn -> fn cir) nmv
  ss = map (\(x,y) -> tikz_of_clifford len x
             ++ tikz_of_clifford_with_labels len y ["|||||"] []) ns
  s = string_of_list "\\m{\n"  "}&\\rightarrow &\n\\m{\n" "}\n" "" show ss
  s' =  string_of_list "\\begin{eqnarray}\n" "\\\\\\nonumber\\\\[0ex]\n" "\\end{eqnarray}" "" show s
