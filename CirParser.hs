{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module CirParser where

--import QuipperLib
--import QuipperLib.Qureg

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import qualified Data.Set as Set
import GateStruct
import Quipper
import Quipper.Monad
import Quipper.Printing
import System.Environment

--a = qinit_register [True]

newtype Parser a = Parser {parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, _)] -> error "Parser did not consume entire stream."
    _ -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c : cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser (\s -> [(a, s)])

instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
  return = unit
  (>>=) = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  item `bind` \c ->
    if p c
      then unit c
      else failure

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do a <- p; rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c : cs) = do char c; string cs; return (c : cs)

token :: Parser a -> Parser a
token p = do a <- p; spaces; return a

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

-------------------------------------------------------------------------------
-- Calulator parser
-------------------------------------------------------------------------------

-- number = [ "-" ] digit { digit }.
-- digit = "0" | "1" | ... | "8" | "9".
-- expr = term { addop term }.
-- term = factor { mulop factor }.
-- factor = "(" expr ")" | number.
-- addop = "+" | "-".
-- mulop = "*".

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving (Show)

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n -> n

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

factor :: Parser Expr
factor =
  int
    <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

run :: String -> Expr
run = runParser expr

pGate :: Parser Gate
pGate =
  pI
    <|> pX
    <|> pY
    <|> pH
    <|> pCnot
    <|> pT
    <|> pT'
    <|> pCS
    <|> pCS'
    <|> pCZ
    <|> pCCZ
    <|> pS
    <|> pS'
    <|> pZ
    <|> pToffoli
    <|> pToffoli4
    <|> pToffolin
    <|> pP
    <|> pM

--gates :: Parser [Gate]

pI = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "I"
  spaces
  target <- number
  spaces
  return (I target)

pX = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "X"
  spaces
  target <- number
  spaces
  return (X target)

pY = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "Y"
  spaces
  target <- number
  spaces
  return (Y target)

pH = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "H"
  spaces
  target <- number
  spaces
  return (H target)

pCnot = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "CNOT"
  spaces
  target <- number
  spaces
  ctrl <- number
  spaces
  return (Cnot target ctrl)

pT = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "T"
  spaces
  target <- number
  spaces
  return (T target)

pT' = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "T'"
  spaces
  target <- number
  spaces
  return (T' target)

pCS = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "CS"
  spaces
  target <- number
  spaces
  ctrl <- number
  spaces
  return (CS target ctrl)

pCS' = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "CS'"
  spaces
  target <- number
  spaces
  ctrl <- number
  spaces
  return (CS' target ctrl)

pCZ = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "CZ"
  spaces
  target <- number
  spaces
  ctrl <- number
  spaces
  return (CZ target ctrl)

pCCZ = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "CCZ"
  spaces
  target <- number
  spaces
  ctrl <- number
  spaces
  ctrl2 <- number
  spaces
  return (CCZ target ctrl ctrl2)

pToffoli = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "Toffoli"
  spaces
  target <- number
  spaces
  ctrl <- number
  spaces
  ctrl2 <- number
  spaces
  return (Toffoli target ctrl ctrl2)

pToffoli4 = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "Toffoli-4"
  spaces
  target <- number
  spaces
  ctrl <- number
  spaces
  ctrl2 <- number
  spaces
  ctrl3 <- number
  spaces
  return (Toffoli4 target ctrl ctrl2 ctrl3)

pToffolin = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "t"
  spaces
  targets <- numbers
  spaces
  return (Toffolin targets)

numbers :: Parser [Int]
numbers = do
  n <- number
  spaces
  ns <- numbers <|> return []
  spaces
  return (n : ns)

pS = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "S"
  spaces
  target <- number
  spaces
  return (S target)

pS' = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "S'"
  spaces
  target <- number
  return (S' target)

pZ = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "Z"
  spaces
  target <- number
  spaces
  return (Z target)

pP = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "P"
  spaces
  target <- numbers
  spaces
  return (P target)

pM = do
  char '['
  ind <- number
  char ']'
  spaces
  char '='
  spaces
  string "M"
  spaces
  target <- number
  spaces
  return (M target)

data Cir = Cir
  { n :: Int,
    nx :: Int,
    ny :: Int,
    nz :: Int,
    m :: Int,
    gates :: [Gate]
  }
  deriving (Show)

pgates :: Parser [Gate]
pgates = do
  g <- pGate
  spaces
  gs <- pgates <|> return []
  spaces
  return (g : gs)

pcir_in :: Parser Cir
pcir_in = do
  many $ satisfy (\x -> x /= ':')
  char ':'
  spaces
  cir <- pcir
  spaces
  many $ satisfy (\x -> x /= ';')
  return cir

pcir_out :: Parser Cir
pcir_out = do
  many $ satisfy (\x -> x /= ':')
  char ':'
  spaces
  many $ satisfy (\x -> x /= ':')
  char ':'
  spaces
  cir <- pcir
  spaces
  many $ satisfy (\x -> x /= ';')
  return cir

pcir :: Parser Cir
pcir = do
  char 'n'
  spaces
  n' <- number
  spaces
  string "n_x"
  spaces
  x <- number
  spaces
  string "n_y"
  spaces
  y <- number
  spaces
  string "n_z"
  spaces
  z <- number
  spaces
  char 'm'
  spaces
  m' <- number
  spaces
  gates' <- pgates
  return Cir {n = n', nx = x, ny = y, nz = z, m = m', gates = gates'}

and_gate :: (Qubit, Qubit) -> Circ (Qubit)
and_gate (a, b) = do
  c <- qinit False
  qnot_at c `controlled` [a, b]
  return c

and_list :: [Qubit] -> Circ Qubit
and_list [] = do
  c <- qinit True
  return c
and_list [q] = do
  return q
and_list (q : t) = do
  d <- and_list t
  e <- and_gate (d, q)
  return e

main' = print_simple PDF and_gate

--main :: IO String
main2 = do
  args <- getArgs
  str <- readFile $ (head args)
  let cir_in = runParser pcir_in str
  let nn_in = n cir_in
  let xn_in = nx cir_in
  let zn_in = nz cir_in
  let qs_in = replicate nn_in qubit
  -- print_generic PDF (\x -> do
  --                       label (drop xn_in x) "|0>"
  --                       foldM gl2cir x (gates cir_in)
  --                       label (drop xn_in x) "|0>"
  --                   ) qs_in
  return $ show cir_in ++ show xn_in
  let cir_out = runParser pcir_out str
  let nn_out = n cir_out
  let xn_out = nx cir_out
  let zn_out = nz cir_out
  let qs_out = replicate nn_out qubit
  -- print_generic PDF (\x -> do
  --                       label (drop xn_out x) "|0>"
  --                       foldM gl2cir x (gates cir_out)
  --                       label (drop xn_out x) "|0>"
  --                   ) qs_out
  return $ show cir_out ++ show xn_out

--  let gl_in = gates cir_in
--  let term = c2term gl_in
--  let t_before = tCount term
--  let term_reduced = heuri term
--  let t_after = tCount term_reduced
--  appendFile (args !! 1) ("\n" ++ prettyprint (drop 7 (head args)) ++ "           " ++ show t_before  ++ "            " ++ show t_after )

main7 = print_generic PDF and_list (replicate 10 qubit)

main'' :: IO ()
main'' = forever $ do
  putStr "> "
  a <- getLine
  print $ eval $ run a

genCir :: Cir -> ([Qubit] -> Circ ())
genCir cir = \qs -> do
  let gl = gates cir

  return ()
