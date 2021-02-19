module Main where

--import CirParser

--import qualified ZX2 as ZX2

import qualified Circuit4 as C
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Fast
import qualified Fast as Fast
import GateStruct
import QCParser2
import Quipper
import Quipper.Libraries.QuipperASCIIParser
import QuipperParser
import System.CPUTime
import System.Environment
import System.IO
import System.Random
import TfcParser2
import qualified ToFile as ToF
import qualified ToQC as QC
import Todd
import qualified ZX8 as ZX

--import Squeeze2

{-
-- tcount
main_tcount = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let term = c2term35 cir_in
  let t_before = tCount term
  let t_before = tCount term
  let term_reduced1 = heuri term
  let term_reduced2 = heuri term_reduced1
  let term_reduced3 = heuri5_dec term
  let term_reduced4 = heuri5 term_reduced1
  let t_after = tCount term_reduced3
  let file_name' = drop 16 file_name
  appendFile (args !! 1) ("\n" ++  take 15 (file_name' ++ "              ") ++ "           " ++ show t_before  ++ "            " ++ show t_after ) --should be after

{-
-- fast tcount
main_fast = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let term = c2term35 cir_in
  let t_before = tCount term
  let term_reduced = Fast.heuri5_fast term
  let t_after = tCount term_reduced
  let file_name' = drop 16 file_name
  appendFile (args !! 1) ("\n" ++  take 15 (file_name' ++ "              ") ++ "           " ++ show t_before  ++ "            " ++ show t_after ) --should be after

-}

-- _wire_change
main_wc = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let w_before = length $ wiresOfCir cir_in
  let term@(LMR l m r) = c2term35 cir_in
  let w_after = length $ wiresOfTerm (l ++ m ++ r)
  let file_name' = drop 16 file_name
  appendFile (args !! 1) ("\n" ++  take 15 (file_name' ++ "              ") ++ "           " ++ show w_before  ++ "            " ++ show w_after  ++ "            " ++ show (w_after-w_before) )

-- main_log
main_log = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let term@(LMR l m r) = c2term35 cir_in
  let (red1, log1) = heuriM term
  let (red2, log2) = heuri5M red2
  let ids_used = log1 ++ log2
  let file_name' = drop 16 file_name
  appendFile (args !! 1) ("\n" ++  take 15 (file_name' ++ "              ") ++ "           "  ++ "            " ++ show ids_used )

main_pdf = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let term = c2term cir_in
  let t_before = tCount term
  let term_reduced = heuri5 term
  let t_after = tCount term_reduced
  let file_name' = drop 16 file_name
--  putStrLn file_name'
--  putStrLn $ show cir_in
  topdf_file $ z2x cir_in
--  appendFile (args !! 1) ("\n" ++  take 15 (file_name' ++ "              ") ++ "           " ++ show t_before  ++ "            " ++ show t_after )

-- | output T-gadget in circuit
main_tg = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let term = c2term35 cir_in
  let t_before = tCount term
  let term_reduced1 = heuri term
  let term_reduced2 = heuri5 term_reduced1
  let t_after = tCount term_reduced2
  let og = mid term_reduced2
  let file_name' = file_name ++ "_TG.tfc"
--  h <- openFile file_name' WriteMode
--  hPutStr h ("tests") --  ++ cir2string (gs2cir og))
  writeFile file_name' (cir2string (gs2cir og))
  let file_name2 = drop 16 file_name
  appendFile (args !! 1) ("\n" ++  take 15 (file_name2 ++ "              ") ++ "           " ++ show t_before  ++ "            " ++ show t_after )

-- | output qc circuit
main_qc = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let term@(LMR lt mt rt) = c2term35 cir_in
  let t_before = tCount term
  let term_reduced1 = heuri term
  let term_reduced2 = heuri5 term_reduced1
  let t_after = tCount term_reduced2
  let term_after@(LMR l m r) = term_reduced2
  let cir_after = (l ++ m ++ r)
  let file_name1 = file_name ++ "_i.qc"
  writeFile file_name1 (QC.cir2string cir_in)
  let file_name2 = file_name ++ "_o.qc"
  writeFile file_name2 (QC.cir2string (QC.gs2cir cir_after))

-- | test mvh
main_mvh = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let cin = [] --ToF.cir2string $  cir_in
  let cio = [] --ToF.cir2string $ ZX.tocir $ ZX.cir2lmmr'' (map C.cnot2cx cir_in) -- >>=ZX.tryIds_eager []
  path <- getExecutablePath
  let fn = takeWhileB (\x -> x /= '/') file_name
  let file_name1 = "Stomp/" ++ fn ++ "_i.qc"
  writeFile file_name1 cin
  let file_name2 = "Stomp/" ++ fn ++ "_o.qc"
  writeFile file_name2 cio

--takeWhileB :: (a -> Bool) -> [a] -> [a]
--takeWhileB p cir = reverse $ takeWhile p ( reverse cir)

-- _wire_change
mainwchange = do
  args <- getArgs
  let file_name = head args
  str <- readFile $ file_name
  let ext = reverse $ take 3 (reverse file_name)
  cir_in <-  case ext of
        ".qc" -> parseQC str
        "tfc" -> parseTfc str
        _ -> parseQuipper str
  let w_before = length $ wiresOfCir cir_in
  let cio = ZX.tocir $ ZX.cir2lmmr' (map C.cnot2cx cir_in)
  let w_after = length $ wiresOfCir cio
  let file_name' = drop 16 file_name
  appendFile (args !! 1) ("\n" ++  take 15 (file_name' ++ "              ") ++ "           " ++ show w_before  ++ "            " ++ show w_after  ++ "            " ++ show (w_after-w_before) )

-}

run :: StdGen -> [(String, String)] -> [String] -> IO ()
run stdgen options (x : []) = do
  let file_name = x
  str <- readFile $ x

  let ext = reverse $ take 3 (reverse file_name)

  --  putStrLn ext

  (ivq, cir_in) <- case ext of
    ".qc" -> parseQC str
    "tfc" -> {-# SCC "parseTfc-" #-} parseTfc str
    _ -> parseQuipper str
  putStrLn (show ivq)

  starts <- getCPUTime

  let homoids = case lookup "-identity" options of
        Nothing -> []
        Just ids -> case ids of
          "4" -> ZX.id4s
          "6" -> ZX.id6s
          "45" -> ZX.id4s ++ ZX.id45s
          "56" -> ZX.id56s
          "96" -> ZX.wid96
          "4-56" -> ZX.id4s ++ ZX.id56s
          "456" -> ZX.id4s ++ ZX.id45s ++ ZX.id6s
          "4567" -> ZX.id4s ++ ZX.id45s ++ ZX.id6s ++ ZX.id67s

  let wireids =
        ( case lookup "-identity" options of
            Nothing -> \f -> return
            Just ids -> case ids of
              "4" -> (\f -> \x -> ((f ZX.id4s) x))
              "6" -> (\f -> \x -> ((f ZX.id6s) x))
              "45" -> (\f -> \x -> (f ZX.id4s) x >>= f ZX.id45s)
              "56" -> (\f -> \x -> (f ZX.id56s) x)
              "96" -> (\f -> \x -> (f ZX.wid96) x)
              "4-56" -> (\f -> \x -> (f ZX.id4s) x >>= f ZX.id56s)
              "456" -> (\f -> \x -> (f ZX.id4s) x >>= f ZX.id45s >>= f ZX.id6s)
              "4567" -> (\f -> \x -> (f ZX.id4s) x >>= f ZX.id45s >>= f ZX.id6s >>= f ZX.id67s)
        ) ::
          ([ZX.Identity] -> ZX.TGCG -> ZX.LMMR) -> ZX.TGCG -> ZX.LMMR
  let order = case lookup "-order" options of
        Nothing -> ""
        Just o -> o

  {-  let cir_s' = case lookup "-identity" options of
          -- only fusion
          Nothing -> {-# SCC "cir2lmmr-" #-} ZX.cir2lmmr' cir_in
          Just ids -> ({-# SCC "cir2lmmr-" #-} ZX.cir2lmmr' cir_in) >>= case order of
              "homo-eager" -> (ZX.runIds_r stdgen homoids)
              "homo" ->  (ZX.runIds_r stdgen homoids)
              "homo-linear" ->  (ZX.runIds_r stdgen homoids)
              "wire-eager" -> (wireids $ ZX.runIds_r stdgen)
              "wire" -> (wireids $ {-# SCC "runIds_r-" #-} ZX.runIds_r stdgen )

  -}
  let cir_s = ((ZX.initLMR' ivq (desugar_cir cir_in)) >>= ZX.cir2lmmr') >>= (ZX.runIds_rw stdgen 20000 (ZX.id4s ++ ZX.zxid45s)) -- >>= (ZX.runIds_r stdgen 5000  (take 127 ZX.id56s ++ ZX.wid96))
  let (af, ((ll, rr), wcw@(vq, fq), tct@(int, fut, idt))) = runState (cir_s >>= ZX.tct) (([], []), (0, 0), (0, 0, 0))
  let cin = ToF.cir2string vq vq cir_in
  let cio = ToF.cir2string vq fq (ll ++ (ZX.gads2cir2 (fst af) ++ (ZX.gads2cir2 (snd af) ++ rr)))
  path <- getExecutablePath
  let fn = takeWhileB (\x -> x /= '/') file_name
  let file_name1 = "Stomp/" ++ fn ++ "_i.qc"
  putStrLn "Hello"
  writeFile file_name1 cin
  let file_name2 = "Stomp/" ++ fn ++ "_o.qc"
  writeFile file_name2 cio
  case lookup "-tcount" options of
    Nothing -> putStrLn "no -tcount option"
    Just tc -> do
      let t_before = int -- ZX.tcount_zx $ ZX.tolxr cir_in
      --   let t_fusion = ZX.tcount $ ZX.cir2lmmr' cir_in
      let t_fusion = fut
      --      let (w_after, t_after) = seq cir_s (length $ filter isInitg $ ZX.tocir cir_s, ZX.tcount $ cir_s)
      let t_after = idt
      let w_after = fq
      let fname' = case length file_name > 150 of
            True -> take 19 (drop 15 file_name ++ "              ")
            False -> take 19 $ file_name ++ "              "
      appendFile (tc) ("\n" ++ fname' ++ "       " ++ show (tct, idt - fut) ++ "        " ++ show (vq, fq, fq - vq))
      putStrLn $ show (tct, idt - fut, (vq, fq, fq - vq))

  --  let result        = stomp options cir_in
  ends <- getCPUTime
  let time = (fromIntegral $ ends - starts) / 10 ^ 12
  let tc = unJust $ lookup "-tcount" options
  appendFile (tc) ("       " ++ show time)
  putStrLn $ "Success (took " ++ take 6 (show time) ++ "s)"
--  putStrLn result

run stdgen options (x : y : xs)
  | x == "-tcount" = run stdgen (("-tcount", y) : options) xs
  | x == "-ancilla-used" = run stdgen (("-ancilla-used", y) : options) xs
  | x == "-order" = run stdgen (("-order", y) : options) xs
  | x == "-identity" = run stdgen (("-identity", y) : options) xs
  | x == "-pdf" = run stdgen (("-pdf", y) : options) xs
run stdgen _ _ = do
  putStrLn "Invalid argument(s)"

main :: IO ()
main = do
  stdgen <- getStdGen
  getArgs >>= run stdgen []

id2qc :: ZX.Identity -> String -> IO ()
id2qc mm fn = do
  let file_name1 = "Ids/" ++ fn ++ "_i.qc"
  let mm' = ZX.id2cir mm
  let lmm' = length (wiresOfCir mm')
  let lmm'' = maximum (wiresOfCir mm') + 1
  writeFile file_name1 $ ToF.cir2string lmm'' lmm'' mm'
