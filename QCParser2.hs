module QCParser2 (parseQC,parseQC')  where
import System.Environment
import Data.Char
import qualified Data.Set as Set 
import Data.List
import qualified GateStruct as GS
import Control.Monad

{-
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
-}

import Text.Parsec hiding (space)
import Text.Parsec.Char hiding (space)
--import Text.Parsec.Number


import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as MS

type GName = String
type GWire = String

data Gate = Gate GName [GWire] deriving (Eq, Show, Ord)

data DotTfc = DotTfc { qubits  :: [GWire],
                     inputs  :: [GWire],
                     outputs :: [GWire],
                     glist   :: [Gate] }


instance  Show DotTfc where
  show (DotTfc q i o glist) = intercalate "\n" (q':i':o':bod)
    where q'  = ".v " ++ showLst q
          i'  = ".i " ++ showLst (filter (`elem` i) q)
          o'  = ".o " ++ showLst (filter (`elem` o) q)
          bod = map show glist
          showLst = intercalate ","

{- Parser -}

space = char ' '
comma = char ','
semicolon = char ';'
sep = space <|> tab <|> comma
comment = char '#' >> manyTill anyChar endOfLine >> return '#'
delimiter = semicolon <|> endOfLine

skipSpace     = skipMany $ sep <|> comment
skipWithBreak = many1 (skipMany sep >> delimiter >> skipMany sep)

parseID = try $ do
  c  <- letter
  cs <- many (alphaNum <|> char '*')
  if (c:cs) == "BEGIN" || (c:cs) == "END" then fail "" else return (c:cs)

parseParams = sepEndBy (many1 alphaNum) (many1 sep) 

{-
parseDiscrete = do
  numerator <- option 1 nat
  string "pi"
  string "/2^"
  power <- int
  return $ Discrete $ dyadic numerator (power+1)

parseContinuous = floating2 True >>= return . Continuous

parseAngle = do
  char '('
  theta <- sign <*> (parseDiscrete <|> parseContinuous)
  char ')'
  return theta
-}

parseGate = do
  name <- parseID
--  param <- optionMaybe parseAngle
--  reps <- option 1 (char '^' >> nat)
  skipSpace
  params <- parseParams
  skipSpace
  return $ Gate name params


parseCir = do
  string "BEGIN"
  skipSpace
  id <- option "main" (try parseID)
  skipSpace
  skipWithBreak
  body <- endBy parseGate skipWithBreak
  string "END"
  return $ body

parseHeaderLine s = do
  string s
  skipSpace
  params <- parseParams
  skipWithBreak
  return params

parseFile = do
  skipMany $ sep <|> comment <|> delimiter
  qubits <- parseHeaderLine ".v"
  inputs <- option qubits $ try $ parseHeaderLine ".i"
  outputs <- option qubits $ try $ parseHeaderLine ".o"
  option qubits $ try $ parseHeaderLine ".c"
  option qubits $ try $ parseHeaderLine ".ov"
  cir <- sepEndBy parseCir skipWithBreak
  skipMany $ sep <|> delimiter
  skipSpace
  eof
  return $ DotTfc qubits (inputs) (outputs) (concat cir)

parseDotTfc :: String -> Either ParseError DotTfc
parseDotTfc = parse parseFile ".qc parser"

unJust (Just a) = a 

d2cir :: DotTfc -> (Int, [GS.Gate])
d2cir (DotTfc q i o glist) = (length q, map g2g glist) where
  qix = MS.fromList (zip q [0..length q - 1])
  ix p = unJust $ MS.lookup p qix
  g2g (Gate "x" ps) = GS.X (ix $ head ps)
  g2g (Gate "X" ps) = GS.X (ix $ head ps)  
  g2g (Gate "z" ps) = GS.Z (ix $ head ps)
  g2g (Gate "Z" ps) = GS.Z (ix $ head ps)  
  g2g (Gate "s" ps) = GS.S (ix $ head ps)
  g2g (Gate "S" ps) = GS.S (ix $ head ps)  
  g2g (Gate "H" ps) = GS.H (ix $ head ps)
  g2g (Gate "not" ps) = GS.X (ix $ head ps)  
  g2g (Gate "cnot" ps) = GS.CX (ix (ps !! 1)) (ix (ps !! 0))
  g2g (Gate "CNOT" ps) = GS.CX (ix (ps !! 1)) (ix (ps !! 0))
  g2g (Gate "CX" ps) = GS.CX (ix (ps !! 1)) (ix (ps !! 0))    
  g2g (Gate "tof" ps) = GS.CCX (ix (ps !! 2)) (ix (ps !! 0)) (ix (ps !! 1))
  g2g (Gate "Tof" ps) = GS.CCX (ix (ps !! 2)) (ix (ps !! 0)) (ix (ps !! 1))  
  g2g (Gate "t3" ps) = GS.CCX (ix (ps !! 2)) (ix (ps !! 0)) (ix (ps !! 1))    

parseQC :: String -> IO (Int, [GS.Gate])
parseQC bs = case parseDotTfc bs of
  Right d -> return $ d2cir d

parseQC' :: String -> IO [GS.Gate]
parseQC' fn = do
  bs <- readFile fn
  (p1,p2) <- parseQC bs
  return $ p2
