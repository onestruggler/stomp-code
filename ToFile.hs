module ToFile where

import Data.List
import Fast
import GateStruct
import System.Environment
import System.IO

-- | show list [1,2,3] to 1 2 3
show_list :: [Int] -> String
show_list [] = "\n"
show_list (h : t) = show h ++ " " ++ show_list t

-- | specify variable in a .qc file
qc_header :: [Int] -> String
qc_header xs = ".v " ++ show_list xs

-- | specify variable in a .qc file
qc_iheader :: Int -> [Gate] -> String
qc_iheader vq xs = ".i " ++ show_list xs'
  where
    xs' = [0 .. vq -1]

gate2string :: Gate -> String
gate2string (Cnot t c) = "cnot " ++ show c ++ " " ++ show t ++ "\n"
gate2string (CX t c) = "cnot " ++ show c ++ " " ++ show t ++ "\n"
gate2string (Swap t c) = "swap " ++ show c ++ " " ++ show t ++ "\n"
gate2string (CZ i j) = "H " ++ show j ++ "\ncnot " ++ show i ++ " " ++ show j ++ "\n" ++ "H " ++ show j ++ "\n"
gate2string (Init _ i) = "H " ++ show i ++ "\n"
gate2string (Term _ i) = "H " ++ show i ++ "\n"
gate2string (T t) = "T " ++ show t ++ "\n"
gate2string (S t) = "S " ++ show t ++ "\n"
gate2string (Z t) = "Z " ++ show t ++ "\n"
gate2string (H t) = "H " ++ show t ++ "\n"
gate2string (X t) = "X " ++ show t ++ "\n"
gate2string (CCZ i j k) = "H " ++ show k ++ "\ntof " ++ show i ++ " " ++ show j ++ " " ++ show k ++ "\n" ++ "H " ++ show k ++ "\n"
gate2string (Toffoli i j k) = "tof " ++ show k ++ " " ++ show j ++ " " ++ show i ++ "\n"
gate2string (CCX i j k) = "tof " ++ show k ++ " " ++ show j ++ " " ++ show i ++ "\n"

cir2string :: Int -> Int -> [Gate] -> String
cir2string vq fq gl = qc_header ([0 .. fq -1]) ++ qc_iheader (vq) gl ++ "BEGIN\n" ++ (concat $ map gate2string gl) ++ "END"

cir2qc' :: Int -> Int -> [Gate] -> IO ()
cir2qc' vq fq xs = putStrLn (cir2string vq fq xs)

cir2qc :: Handle -> Int -> Int -> [Gate] -> IO ()
cir2qc h vq fq xs = hPutStrLn h (cir2string vq fq xs)

main = do
  args <- getArgs
  let file_name = head args
  let format = args !! 1
  h <- openFile (file_name ++ ("." ++ format)) WriteMode
  case format of
    "QC" -> cir2qc h 10 10 []
    "Tfc" -> cir2qc h 10 10 []
