module Todd where

import Data.List
import Fast
import GateStruct
import System.Environment
import System.IO

i2st :: Int -> Int -> [Gate]
i2st 1 j = [T j]
i2st 3 j = [T j, S j]
i2st 5 j = [T j, S j, S j]
i2st 7 j = [T j, S j, S j, S j]
i2st 2 j = [S j]
i2st 4 j = [S j, S j]
i2st 6 j = [S j, S j, S j]
i2st 0 j = []

stepcnot :: [Int] -> [Gate]
stepcnot [a, b] = [Cnot b a]
stepcnot (a : b : c : t) = Cnot (last (c : t)) a : stepcnot (b : c : t)

g2cir :: ZXAtom -> [Gate]
g2cir (CnotG i j) = [Cnot i j]
g2cir (SwapG i j) = [Swap i j]
g2cir (CZG i j) = [H i, Cnot i j, H i]
g2cir (HG i) = [H i]
g2cir (XG i) = [X i]
g2cir (SG i) = [S i]
g2cir (ZG i) = [Z i]
g2cir (InitG s i) = [Init s i, H i, S i, S i, S i]
g2cir (TermG s i) = [S i, S i, S i, H i]
g2cir (G i ws) = case length ws of
  1 -> i2st i (maximum ws)
  _ -> casl ++ sts ++ casr
  where
    casl = stepcnot ws
    casr = reverse casl
    sts = i2st i (last ws)

gs2cir :: [ZXAtom] -> [Gate]
gs2cir xs = concatMap g2cir xs

term2cir :: Term -> [Gate]
term2cir (LMR l m r) = concatMap g2cir (l ++ m ++ r)

-- | show list [1,2,3] to 1 2 3
show_list :: [Int] -> String
show_list [] = "\n"
show_list (h : t) = show h ++ " " ++ show_list t

-- | specify variable in a .qc file
qc_header :: [Int] -> String
qc_header xs = ".v " ++ show_list xs

-- | specify variable in a .qc file
qc_iheader :: [Gate] -> String
qc_iheader xs = ".i " ++ show_list xs'
  where
    xs' = wiresOfCir xs \\ wiresOfCir xs1
    xs1 = filter isInitg xs

gate2string :: Gate -> String
gate2string (Cnot t c) = "t2 " ++ show c ++ " " ++ show t ++ "\n"
gate2string (Swap t c) = "swap " ++ show c ++ " " ++ show t ++ "\n"
gate2string (CZ i j) = "H " ++ show i ++ "\ncnot " ++ show i ++ " " ++ show j ++ "\n" ++ "H " ++ show i ++ "\n"
gate2string (Init _ _) = ""
gate2string (T t) = "T " ++ show t ++ "\n"
gate2string (S t) = "S " ++ show t ++ "\n"
gate2string (Z t) = "Z " ++ show t ++ "\n"
gate2string (H t) = "H " ++ show t ++ "\n"
gate2string (Z t) = "Z " ++ show t ++ "\n"
gate2string (X t) = "X " ++ show t ++ "\n"
gate2string (CCZ i j k) = "H " ++ show k ++ "\ntof " ++ show i ++ " " ++ show j ++ " " ++ show k ++ "\n" ++ "H " ++ show k ++ "\n"
gate2string (Toffoli i j k) = "tof " ++ show k ++ " " ++ show j ++ " " ++ show i ++ "\n"

cir2string :: [Gate] -> String
cir2string gl = qc_header (wiresOfCir gl) ++ qc_iheader gl ++ "BEGIN\n" ++ concat (map gate2string gl) ++ "END"

cir2qc :: Handle -> [Gate] -> IO ()
cir2qc h xs = hPutStrLn h (cir2string xs)

maintodd = do
  args <- getArgs
  let file_name = head args
  h <- openFile file_name WriteMode
  cir2qc h []
