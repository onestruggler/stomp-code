{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module TfcParser (parseTfc, parseTfc') where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import GateStruct
import System.Environment
--import Control.Applicative

import Text.ParserCombinators.ReadP

next_word :: ReadP String
next_word = many $ satisfy (\x -> notElem x ", \n\r")

replace :: Char -> Char -> String -> String
replace a b [] = []
replace a b (h : t) = if h == a then b : replace a b t else h : replace a b t

qGate :: [String] -> ReadP Gate
qGate s =
  qt7 s
    <++ qt6 s
    <++ qt5 s
    <++ qt4 s
    <++ qt3 s
    <++ qt2 s
    <++ qt1 s
    <++ qTT s
    <++ qSS s

qSS s = do
  skipSpaces
  string "S"
  skipSpaces
  target <- next_word
  return (S (unJust (target `elemIndex` s)))

qTT s = do
  skipSpaces
  string "T"
  skipSpaces
  target <- next_word
  return (T (unJust (target `elemIndex` s)))

qt1 s = do
  skipSpaces
  string "t1"
  skipSpaces
  target <- next_word
  return (X (unJust (target `elemIndex` s)))

qt2 s = do
  skipSpaces
  string "t2"
  skipSpaces
  c1 <- next_word
  string ","
  target <- next_word
  return (CX (unJust (target `elemIndex` s)) (unJust (c1 `elemIndex` s)))

--  return (Cnot (unJust (target `elemIndex` s)) (unJust (c1 `elemIndex` s)) )
qt3 s = do
  skipSpaces
  string "t3"
  skipSpaces
  c1 <- next_word
  string ","
  c2 <- next_word
  string ","
  target <- next_word
  return (Toffoli (unJust (target `elemIndex` s)) (unJust (c1 `elemIndex` s)) (unJust (c2 `elemIndex` s)))

qt4 s = do
  skipSpaces
  string "t4"
  skipSpaces
  c1 <- next_word
  string ","
  c2 <- next_word
  string ","
  c3 <- next_word
  string ","
  target <- next_word
  return
    ( Toffolin
        [ unJust (target `elemIndex` s),
          unJust (c1 `elemIndex` s),
          unJust (c2 `elemIndex` s),
          unJust (c3 `elemIndex` s)
        ]
    )

qt5 s = do
  skipSpaces
  string "t5"
  skipSpaces
  c1 <- next_word
  string ","
  c2 <- next_word
  string ","
  c3 <- next_word
  string ","
  c4 <- next_word
  string ","
  target <- next_word
  return (Toffolin [unJust (target `elemIndex` s), unJust (c1 `elemIndex` s), unJust (c2 `elemIndex` s), unJust (c3 `elemIndex` s), unJust (c4 `elemIndex` s)])

qt6 s = do
  skipSpaces
  string "t6"
  skipSpaces
  c1 <- next_word
  string ","
  c2 <- next_word
  string ","
  c3 <- next_word
  string ","
  c4 <- next_word
  string ","
  c5 <- next_word
  string ","
  target <- next_word
  return (Toffolin [unJust (target `elemIndex` s), unJust (c1 `elemIndex` s), unJust (c2 `elemIndex` s), unJust (c3 `elemIndex` s), unJust (c4 `elemIndex` s), unJust (c5 `elemIndex` s)])

qt7 s = do
  skipSpaces
  string "t7"
  skipSpaces
  c1 <- next_word
  string ","
  c2 <- next_word
  string ","
  c3 <- next_word
  string ","
  c4 <- next_word
  string ","
  c5 <- next_word
  string ","
  c6 <- next_word
  string ","
  target <- next_word
  return (Toffolin [unJust (target `elemIndex` s), unJust (c1 `elemIndex` s), unJust (c2 `elemIndex` s), unJust (c3 `elemIndex` s), unJust (c4 `elemIndex` s), unJust (c5 `elemIndex` s), unJust (c6 `elemIndex` s)])

qS s = do
  skipSpaces
  string "S"
  skipSpaces
  target <- next_word
  skipSpaces
  return (S (unJust (target `elemIndex` s)))

unJust (Just a) = a

{-
qS' s = do
  skipSpaces
  string "S*"
  skipSpaces
  target <- next_word
  skipSpaces
  return (S' (unJust (target `elemIndex` s)))
-}
qT s = do
  skipSpaces
  string "T"
  skipSpaces
  target <- next_word
  skipSpaces
  return (T (unJust (target `elemIndex` s)))

qX s = do
  skipSpaces
  string "X"
  skipSpaces
  target <- next_word
  skipSpaces
  return (X (unJust (target `elemIndex` s)))

qZ s = do
  skipSpaces
  string "Z"
  skipSpaces
  target <- next_word
  skipSpaces
  return (Z (unJust (target `elemIndex` s)))

{-
qT' s = do
  skipSpaces
  string "T*"
  skipSpaces
  target <- next_word
  skipSpaces
  return (T' (unJust (target `elemIndex` s)))
-}
qH s = do
  skipSpaces
  string "H"
  skipSpaces
  target <- next_word
  skipSpaces
  return (H (unJust (target `elemIndex` s)))

qCnot1 s = do
  skipSpaces
  string "cnot"
  skipSpaces
  control <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (Cnot (unJust (target `elemIndex` s)) (unJust (control `elemIndex` s)))

qCnot2 s = do
  skipSpaces
  string "Cnot"
  skipSpaces
  control <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (Cnot (unJust (target `elemIndex` s)) (unJust (control `elemIndex` s)))

qCnot3 s = do
  skipSpaces
  string "CNOT"
  skipSpaces
  control <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (Cnot (unJust (target `elemIndex` s)) (unJust (control `elemIndex` s)))

qCnot4 s = do
  skipSpaces
  string "CNot"
  skipSpaces
  control <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (Cnot (unJust (target `elemIndex` s)) (unJust (control `elemIndex` s)))

qTof s = do
  skipSpaces
  string "Tof"
  skipSpaces
  control1 <- next_word
  skipSpaces
  control2 <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (Toffoli (unJust (target `elemIndex` s)) (unJust (control1 `elemIndex` s)) (unJust (control2 `elemIndex` s)))

-- qTof4 s = do
--   skipSpaces
--   string "Tof"
--   skipSpaces
--   control1 <- next_word
--   skipSpaces
--   control2 <- next_word
--   skipSpaces
--   control3 <- next_word
--   skipSpaces
--   target <- next_word
--   skipSpaces
--   return (Toffoli (unJust (target `elemIndex` s)) (unJust (control1 `elemIndex` s)) (unJust (control2 `elemIndex` s)) (unJust (control3 `elemIndex` s)))

qCZ s = do
  skipSpaces
  string "CZ"
  skipSpaces
  control <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (CZ (unJust (target `elemIndex` s)) (unJust (control `elemIndex` s)))

names_v :: ReadP [String]
names_v = do
  char '.'
  char 'v'
  skipSpaces
  str <- many $ satisfy (\x -> x /= '.')
  return $ if str /= "BEGIN" then words str else error "parse error, .v line is no good!"

dotv :: ReadP String
dotv = do
  char '.'
  char 'v'
  return ".v"

names_v' :: ReadP [String]
names_v' = do
  manyTill (satisfy (const True)) dotv
  skipSpaces
  str <- many $ satisfy (\x -> x /= '.' && x /= 'B')
  many $ satisfy (\x -> x /= 'B')
  return $ words $ replace ',' ' ' (unwords $ words str)

names_v1 :: ReadP [String]
names_v1 = do
  char '.'
  char 'v'
  skipSpaces
  str <- many $ satisfy (\x -> x /= '.' && x /= 'B')
  return $ words $ replace ',' ' ' (unwords $ words str)

header :: ReadP [String]
header = do
  names_v

qgates :: [String] -> ReadP [Gate]
qgates s = do
  g <- qGate s
  skipSpaces
  gs <- qgates s <++ return []
  skipSpaces
  return (g : gs)

qgates1 :: [String] -> ReadP [Gate]
qgates1 s = do
  g <- qGate s
  skipSpaces
  gs <- qgates s <++ return []
  skipSpaces
  return (g : gs)

qcir = do
  s <- names_v'
  string "BEGIN"
  gl <- qgates s
  skipSpaces
  string "END"
  skipSpaces
  return gl

qcir1 = do
  s <- names_v1
  string "BEGIN"
  gl <- many (qGate s)
  skipSpaces
  string "END"
  skipSpaces
  return gl

parseTfc :: String -> IO [Gate]
parseTfc str = do
  --  str <- readFile $ s
  let (gl, re) = head $ readP_to_S qcir1 str
  let glok = if re /= [] then error ("parse error: " ++ re) else gl
  --putStrLn str
  return glok

parseTfc' :: String -> IO [Gate]
parseTfc' s = do
  str <- readFile s
  let (gl, re) = head $ readP_to_S qcir1 str
  let glok = if re /= [] then error ("parse error: " ++ re) else gl
  --putStrLn str
  return glok

readTfc :: IO String
readTfc = do
  str <- readFile "qc.qc"
  let (gl, re) = head $ readP_to_S qcir str
  return str
