module QCParser (parseQC, parseQC') where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import GateStruct
import System.Environment
--import Control.Applicative

import Text.ParserCombinators.ReadP

next_word :: ReadP String
next_word = many $ satisfy (`notElem` " \n\r")

qGate :: [String] -> ReadP Gate
qGate s =
  qCCZ s
    <++ qtof2 s
    --  <++ ( qCnot1 s)
    <++ qCnot3 s
    <++ qX s
    --  <++ ( qNot s)
    <++ qZ s
    <++ qT s
    <++ qH s
    <++ qS s

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

qNot s = do
  skipSpaces
  string "not"
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
  return (CX (unJust (target `elemIndex` s)) (unJust (control `elemIndex` s)))

qtof2 s = do
  skipSpaces
  string "tof"
  skipSpaces
  control <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (CX (unJust (target `elemIndex` s)) (unJust (control `elemIndex` s)))

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

qtof s = do
  skipSpaces
  string "tof"
  skipSpaces
  control1 <- next_word
  skipSpaces
  control2 <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (Toffoli (unJust (target `elemIndex` s)) (unJust (control1 `elemIndex` s)) (unJust (control2 `elemIndex` s)))

qCCZ s = do
  skipSpaces
  string "Z"
  skipSpaces
  control1 <- next_word
  skipSpaces
  control2 <- next_word
  skipSpaces
  target <- next_word
  skipSpaces
  return (CCZ (unJust (target `elemIndex` s)) (unJust (control1 `elemIndex` s)) (unJust (control2 `elemIndex` s)))

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
  str <- many $ satisfy (/= '.')
  return $ if str /= "BEGIN" then words str else error "parse error, .v line is no good!"

names_v' :: ReadP [String]
names_v' = do
  char '.'
  char 'v'
  skipSpaces
  words <$> many (satisfy (\x -> x /= '.' && x /= 'B'))

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

qcir = do
  s <- names_v'
  skipSpaces
  string "BEGIN"
  skipSpaces
  gl <- qgates s
  skipSpaces
  string "END"
  return gl

names_v1 :: ReadP [String]
names_v1 = do
  char '.'
  char 'v'
  skipSpaces
  words <$> many (satisfy (\x -> x /= '.' && x /= 'B'))

qcir1 = do
  s <- names_v1
  string "BEGIN"
  gl <- many (qGate s)
  skipSpaces
  string "END"
  skipSpaces
  return gl

parseQC :: String -> IO [Gate]
parseQC str = do
  --  str <- readFile $ s
  let (gl, re) = head $ readP_to_S qcir1 str
  --  putStrLn str
  return gl

parseQC2 :: String -> IO (Int, [Gate])
parseQC2 str = do
--  str <- readFile $ s
  let (gl,re) = head $ (readP_to_S qcir1) str
  let n = length $ wiresOfCir gl
  putStrLn (show gl)
  return $ (n,gl)



parseQC' :: String -> IO (Int, [Gate])
parseQC' s = do
  str <- readFile s
  let (gl, re) = head $ readP_to_S qcir1 str
  --  putStrLn str
  return gl

parseQC'2 :: String -> IO (Int, [Gate])
parseQC'2 s = do
  str <- readFile $ s
  let (gl,re) = head $ (readP_to_S qcir1) str
  let n = length $ wiresOfCir gl
--  putStrLn str
  return $ (n,gl)

readQC :: IO String
readQC = do
  str <- readFile "qc.qc"
  let (gl, re) = head $ readP_to_S qcir str
  return str
