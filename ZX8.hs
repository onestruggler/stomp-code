{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module ZX8 where

--import qualified Data.Map.Strict as MS

import qualified Circuit4 as C
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.Distribution.Core
import Data.Distribution.Sample
import Data.Function
import qualified Data.HashMap.Strict as MS
import qualified Data.HashTable.ST.Basic as HT
import Data.Hashable
import Data.List
import Data.Ratio
import qualified Data.Set as Set
import qualified Fast as F
import GHC.Generics
import GateStruct
import QCParser
import QuipperParser
import qualified Squeeze as S1
import qualified Squeeze3 as S
import System.Random
import TfcParser2
import qualified ToFile as ToF

id56s :: [Identity]
id56s = map toId id56s'
  where
    id56s' = map (map (\(F.G i ws) -> (sfl ws, i))) (F.id56s [0 .. 5])

id67s :: [Identity]
id67s = map toId id67s'
  where
    id67s' = map (map (\(F.G i ws) -> (sfl ws, i))) (F.id67s [0 .. 6])

id6s :: [Identity]
id6s = map toId id6s'
  where
    id6s' = map (map (\(F.G i ws) -> (sfl ws, i))) (drop 1 $ F.id6s [0 .. 5])

id45s :: [Identity]
id45s = map toId id45s'
  where
    id45s' = map (map (\(F.G i ws) -> (sfl ws, i))) F.i45eff

id4s :: [Identity]
id4s = [toId $ idFunOn4Wires [0, 1, 2, 3]]

-- | when calling this function, make sure there are 4 distinct indexes in the
-- first argument.
idFunOn4Wires :: [Int] -> [Gadget]
idFunOn4Wires inds =
  [(sfl [x], 1) | x <- inds]
    ++ [(sfl xs, 7) | xs <- F.choosen 2 inds]
    ++ [(sfl xs, 1) | xs <- F.choosen 3 inds]
    ++ [(sfl inds, 7)]

-- | decompose a gadget with higher arity to a list of gadget with
-- arity 1, 2, and 3.
decompG :: Int -> Gate -> [Gate]
decompG nw (Ga p ws)
  | Set.size ws <= nw = [Ga p ws]
  | otherwise = gs
  where
    n = Set.size ws
    inds = Set.toList ws
    gs =
      [Ga (((n -2) * (n -3) `div` 2) `mod` 8) (sfl [x]) | x <- inds]
        ++ [Ga ((- (n -3)) `mod` 8) (sfl xs) | xs <- F.choosen 2 inds]
        ++ [Ga 1 (sfl xs) | xs <- F.choosen 3 inds]

decompGad :: Int -> [Gate] -> LMR
decompGad n cir = return $ concatMap (decompG n) cir

gad2gate :: Gadget -> Gate
gad2gate (k, v) = Ga v k

gate2gad :: Gate -> Gadget
gate2gad (Ga v k) = (k, v)

--gads2cir' gs = map (gad2gate) (MS.toList gs)
id2cir' (l, r) = gads2cir' $ MS.toList l ++ MS.toList r

id2cir (l, r) = gads2cir $ MS.toList l ++ MS.toList r

wid96 = take 8 wid96'

wid96' :: [Identity]
wid96' = ids
  where
    ids1 = map toId [idn 4, idn 5, idn 6, idn 7]
    ids3 =
      (toId $ idn 5 ++ idn 4 ++ idnw [1 .. 4]) : map idn3 [6 .. 25]
    idsb = [idsb5, idsb6, idsb7, idsb8]
    ids' = ids1 ++ idsb ++ ids3
    ids = sortBy (compare `on` (tcount . return)) ids'

wid49 :: [Identity]
wid49 = ids
  where
    ids1 = map toId [idn 4, idn 5, idn 6, idn 7]
    ids3 =
      [idn3 6, idn3 7, idn3 8, idn3 9, idn3 10]
        ++ [toId $ idn 5 ++ idn 4 ++ idnw [1 .. 4]]
    idsb = [idsb5, idsb6, idsb7, idsb8]
    ids = ids1 ++ idsb ++ ids3

idsb8 = toId $ map gate2gad [Ga 1 (sfl [0]), Ga 1 (sfl [1]), Ga 2 (sfl [2]), Ga 2 (sfl [3]), Ga 2 (sfl [4]), Ga 2 (sfl [5]), Ga 2 (sfl [6]), Ga 5 (sfl [7]), Ga 7 (sfl [0, 1]), Ga 7 (sfl [0, 2]), Ga 7 (sfl [0, 3]), Ga 7 (sfl [0, 4]), Ga 7 (sfl [0, 5]), Ga 7 (sfl [0, 6]), Ga 3 (sfl [0, 7]), Ga 7 (sfl [1, 2]), Ga 7 (sfl [1, 3]), Ga 7 (sfl [1, 4]), Ga 7 (sfl [1, 5]), Ga 7 (sfl [1, 6]), Ga 3 (sfl [1, 7]), Ga 2 (sfl [2, 3]), Ga 2 (sfl [2, 4]), Ga 2 (sfl [2, 5]), Ga 2 (sfl [2, 6]), Ga 0 (sfl [2, 7]), Ga 2 (sfl [3, 4]), Ga 2 (sfl [3, 5]), Ga 2 (sfl [3, 6]), Ga 0 (sfl [3, 7]), Ga 2 (sfl [4, 5]), Ga 2 (sfl [4, 6]), Ga 0 (sfl [4, 7]), Ga 2 (sfl [5, 6]), Ga 0 (sfl [5, 7]), Ga 0 (sfl [6, 7]), Ga 2 (sfl [0, 1, 2]), Ga 2 (sfl [0, 1, 3]), Ga 2 (sfl [0, 1, 4]), Ga 2 (sfl [0, 1, 5]), Ga 2 (sfl [0, 1, 6]), Ga 1 (sfl [0, 1, 7]), Ga 2 (sfl [0, 2, 3]), Ga 2 (sfl [0, 2, 4]), Ga 2 (sfl [0, 2, 5]), Ga 2 (sfl [0, 2, 6]), Ga 1 (sfl [0, 2, 7]), Ga 2 (sfl [0, 3, 4]), Ga 2 (sfl [0, 3, 5]), Ga 2 (sfl [0, 3, 6]), Ga 1 (sfl [0, 3, 7]), Ga 2 (sfl [0, 4, 5]), Ga 2 (sfl [0, 4, 6]), Ga 1 (sfl [0, 4, 7]), Ga 2 (sfl [0, 5, 6]), Ga 1 (sfl [0, 5, 7]), Ga 1 (sfl [0, 6, 7]), Ga 2 (sfl [1, 2, 3]), Ga 2 (sfl [1, 2, 4]), Ga 2 (sfl [1, 2, 5]), Ga 2 (sfl [1, 2, 6]), Ga 1 (sfl [1, 2, 7]), Ga 2 (sfl [1, 3, 4]), Ga 2 (sfl [1, 3, 5]), Ga 2 (sfl [1, 3, 6]), Ga 1 (sfl [1, 3, 7]), Ga 2 (sfl [1, 4, 5]), Ga 2 (sfl [1, 4, 6]), Ga 1 (sfl [1, 4, 7]), Ga 2 (sfl [1, 5, 6]), Ga 1 (sfl [1, 5, 7]), Ga 1 (sfl [1, 6, 7]), Ga 4 (sfl [2, 3, 4]), Ga 4 (sfl [2, 3, 5]), Ga 4 (sfl [2, 3, 6]), Ga 2 (sfl [2, 3, 7]), Ga 4 (sfl [2, 4, 5]), Ga 4 (sfl [2, 4, 6]), Ga 2 (sfl [2, 4, 7]), Ga 4 (sfl [2, 5, 6]), Ga 2 (sfl [2, 5, 7]), Ga 2 (sfl [2, 6, 7]), Ga 4 (sfl [3, 4, 5]), Ga 4 (sfl [3, 4, 6]), Ga 2 (sfl [3, 4, 7]), Ga 4 (sfl [3, 5, 6]), Ga 2 (sfl [3, 5, 7]), Ga 2 (sfl [3, 6, 7]), Ga 4 (sfl [4, 5, 6]), Ga 2 (sfl [4, 5, 7]), Ga 2 (sfl [4, 6, 7]), Ga 2 (sfl [5, 6, 7]), Ga 7 (sfl [2, 3, 4, 5, 6]), Ga 7 (sfl [2, 3, 4, 5, 6, 7]), Ga 7 (sfl [0, 1, 2, 3, 4, 5, 6]), Ga 7 (sfl [0, 1, 2, 3, 4, 5, 6, 7])]

idsb6 = toId $ map gate2gad [Ga 7 (sfl [0]), Ga 2 (sfl [1]), Ga 2 (sfl [2]), Ga 2 (sfl [3]), Ga 1 (sfl [4]), Ga 1 (sfl [5]), Ga 4 (sfl [0, 1]), Ga 4 (sfl [0, 2]), Ga 4 (sfl [0, 3]), Ga 5 (sfl [0, 4]), Ga 5 (sfl [0, 5]), Ga 2 (sfl [1, 2]), Ga 2 (sfl [1, 3]), Ga 3 (sfl [1, 4]), Ga 3 (sfl [1, 5]), Ga 2 (sfl [2, 3]), Ga 3 (sfl [2, 4]), Ga 3 (sfl [2, 5]), Ga 3 (sfl [3, 4]), Ga 3 (sfl [3, 5]), Ga 3 (sfl [4, 5]), Ga 2 (sfl [0, 1, 2]), Ga 2 (sfl [0, 1, 3]), Ga 1 (sfl [0, 1, 4]), Ga 1 (sfl [0, 1, 5]), Ga 2 (sfl [0, 2, 3]), Ga 1 (sfl [0, 2, 4]), Ga 1 (sfl [0, 2, 5]), Ga 1 (sfl [0, 3, 4]), Ga 1 (sfl [0, 3, 5]), Ga 1 (sfl [0, 4, 5]), Ga 3 (sfl [1, 2, 3]), Ga 2 (sfl [1, 2, 4]), Ga 2 (sfl [1, 2, 5]), Ga 2 (sfl [1, 3, 4]), Ga 2 (sfl [1, 3, 5]), Ga 2 (sfl [1, 4, 5]), Ga 2 (sfl [2, 3, 4]), Ga 2 (sfl [2, 3, 5]), Ga 2 (sfl [2, 4, 5]), Ga 2 (sfl [3, 4, 5]), Ga 7 (sfl [0, 1, 2, 3]), Ga 7 (sfl [1, 2, 3, 4, 5]), Ga 7 (sfl [0, 1, 2, 3, 4, 5])]

idsb5 = toId $ map gate2gad [Ga 4 (sfl [0]), Ga 4 (sfl [1]), Ga 4 (sfl [2]), Ga 4 (sfl [3]), Ga 3 (sfl [4]), Ga 5 (sfl [0, 1]), Ga 5 (sfl [0, 2]), Ga 5 (sfl [0, 3]), Ga 6 (sfl [0, 4]), Ga 5 (sfl [1, 2]), Ga 5 (sfl [1, 3]), Ga 6 (sfl [1, 4]), Ga 5 (sfl [2, 3]), Ga 6 (sfl [2, 4]), Ga 6 (sfl [3, 4]), Ga 2 (sfl [0, 1, 2]), Ga 2 (sfl [0, 1, 3]), Ga 1 (sfl [0, 1, 4]), Ga 2 (sfl [0, 2, 3]), Ga 1 (sfl [0, 2, 4]), Ga 1 (sfl [0, 3, 4]), Ga 2 (sfl [1, 2, 3]), Ga 1 (sfl [1, 2, 4]), Ga 1 (sfl [1, 3, 4]), Ga 1 (sfl [2, 3, 4]), Ga 7 (sfl [0, 1, 2, 3]), Ga 7 (sfl [0, 1, 2, 3, 4])]

idsb7 = toId $ map gate2gad [Ga 5 (sfl [0]), Ga 4 (sfl [1]), Ga 4 (sfl [2]), Ga 4 (sfl [3]), Ga 4 (sfl [4]), Ga 0 (sfl [5]), Ga 0 (sfl [6]), Ga 2 (sfl [0, 1]), Ga 2 (sfl [0, 2]), Ga 2 (sfl [0, 3]), Ga 2 (sfl [0, 4]), Ga 4 (sfl [0, 5]), Ga 4 (sfl [0, 6]), Ga 6 (sfl [1, 2]), Ga 6 (sfl [1, 3]), Ga 6 (sfl [1, 4]), Ga 1 (sfl [1, 5]), Ga 1 (sfl [1, 6]), Ga 6 (sfl [2, 3]), Ga 6 (sfl [2, 4]), Ga 1 (sfl [2, 5]), Ga 1 (sfl [2, 6]), Ga 6 (sfl [3, 4]), Ga 1 (sfl [3, 5]), Ga 1 (sfl [3, 6]), Ga 1 (sfl [4, 5]), Ga 1 (sfl [4, 6]), Ga 1 (sfl [5, 6]), Ga 2 (sfl [0, 1, 2]), Ga 2 (sfl [0, 1, 3]), Ga 2 (sfl [0, 1, 4]), Ga 1 (sfl [0, 1, 5]), Ga 1 (sfl [0, 1, 6]), Ga 2 (sfl [0, 2, 3]), Ga 2 (sfl [0, 2, 4]), Ga 1 (sfl [0, 2, 5]), Ga 1 (sfl [0, 2, 6]), Ga 2 (sfl [0, 3, 4]), Ga 1 (sfl [0, 3, 5]), Ga 1 (sfl [0, 3, 6]), Ga 1 (sfl [0, 4, 5]), Ga 1 (sfl [0, 4, 6]), Ga 1 (sfl [0, 5, 6]), Ga 4 (sfl [1, 2, 3]), Ga 4 (sfl [1, 2, 4]), Ga 2 (sfl [1, 2, 5]), Ga 2 (sfl [1, 2, 6]), Ga 4 (sfl [1, 3, 4]), Ga 2 (sfl [1, 3, 5]), Ga 2 (sfl [1, 3, 6]), Ga 2 (sfl [1, 4, 5]), Ga 2 (sfl [1, 4, 6]), Ga 2 (sfl [1, 5, 6]), Ga 4 (sfl [2, 3, 4]), Ga 2 (sfl [2, 3, 5]), Ga 2 (sfl [2, 3, 6]), Ga 2 (sfl [2, 4, 5]), Ga 2 (sfl [2, 4, 6]), Ga 2 (sfl [2, 5, 6]), Ga 2 (sfl [3, 4, 5]), Ga 2 (sfl [3, 4, 6]), Ga 2 (sfl [3, 5, 6]), Ga 2 (sfl [4, 5, 6]), Ga 7 (sfl [1, 2, 3, 4]), Ga 7 (sfl [0, 1, 2, 3, 4]), Ga 7 (sfl [1, 2, 3, 4, 5, 6]), Ga 7 (sfl [0, 1, 2, 3, 4, 5, 6])]

idn2 n = ids'
  where
    inds = [0 .. n]
    id1s = concat [idnw ws | ws <- F.choosen n inds]
    id2 = idn (n + 1)
    ids' = id2 ++ id1s

idn3 n' = ids'
  where
    n = n' - 1
    inds1 = [0 .. n]
    inds2 = [1 .. n]
    inds3 = [0 .. n -1]
    inds4 = [1 .. n -1]
    idss = idnw inds1 ++ idnw inds2 ++ idnw inds3 ++ idnw inds4
    ids' = toId idss

idn4 n = ids'
  where
    inds1 = [0 .. n]
    inds2 = [1 .. n]
    inds3 = [0 .. n -1]
    inds4 = [1 .. n -1]
    idss = idnw inds1 ++ idnw inds2 ++ idnw inds3 ++ idnw inds4
    ids' = toId idss

--idn :: Int -> Identity
idn n = ids'
  where
    inds = [0 .. n -1]
    ids' =
      [(sfl [x], (((n -2) * (n -3)) `div` 2) `mod` 8) | x <- inds]
        ++ [(sfl xs, (- (n -3)) `mod` 8) | xs <- F.choosen 2 inds]
        ++ [(sfl xs, 1) | xs <- F.choosen 3 inds]
        ++ [(sfl inds, 7)]

--idn :: Int -> Identity
idnw inds = ids'
  where
    n = length inds
    ids' = if n >= 4 then ids'' else []
    ids'' =
      [(sfl [x], ((n -2) * (n -3) `div` 2) `mod` 8) | x <- inds]
        ++ [(sfl xs, (- (n -3)) `mod` 8) | xs <- F.choosen 2 inds]
        ++ [(sfl xs, 1) | xs <- F.choosen 3 inds]
        ++ [(sfl inds, 7)]

-- | Invetible class
class Inv a where
  inv :: a -> [a]

instance (Inv a) => Inv [a] where
  inv cir = [concatMap inv (reverse cir)]

inv_cir :: [Gate] -> [Gate]
inv_cir cir = concatMap inv_g (reverse cir)

inv_g (T i) = [Z i, S i, T i]
inv_g (S i) = [Z i, S i]
inv_g (Z i) = [Z i]
inv_g (CZ i j) = [CZ i j]
inv_g (CCZ i j k) = [CCZ i j k]
inv_g (CCX i j k) = [CCX i j k]
inv_g (X i) = [X i]
inv_g (CX i j) = [CX i j]
inv_g (Cnot i j) = [CX i j]
inv_g (Swap i j) = [Swap i j]
inv_g (H i) = [H i]
inv_g (Ga i ws) = [Ga (- i `mod` 8) ws]

type Key = Set.Set Int

type Wires = Key

type Phase = Int

type Gadget = (Wires, Phase)

type Gads = MS.HashMap Wires Phase

instance Hashable Key where
  hashWithSalt s k = hashWithSalt s (Set.toList k)

instance Ord Key where
  compare x y
    | Set.size x == Set.size y = compare (Set.toList x) (Set.toList y)
    | otherwise = compare (Set.size x) (Set.size y)

instance Inv Gadget where
  inv (ws, p) = [(ws, (- p) `mod` 8)]

-- | This performs "fusion" --- add the phases (mod 8) of two gadgets with
-- same wires.
toGads :: [Gadget] -> Gads
toGads = MS.fromListWith (\x y -> (x + y) `mod` 8)

gad :: Int -> [Int] -> Gate
gad i ws = Ga i (sfl ws)

-- | We need to break a circuit into 3 parts left, middile, and right.
-- | Int is used to provide fresh name for ancila, and record how many
-- qubits of a input circuit has. the last 3 Ints record the tcounts
-- of input, after fusion, after using identities.
type LMR' a = State (([Gate], [Gate]), (Int, Int), (Int, Int, Int)) a

type LMR = LMR' [Gate]

type TGCG = (Gads, Gads)

type LGR = LMR' Gads

type LMMR = LMR' TGCG

-- | identity (t,c) left is T gadget and right is Clifford gadget
type Identity = TGCG

type Identity' = (Int, TGCG)

-- | transform a list of gadgets (that equals identity) to TGCG Identity.
toId :: [Gadget] -> Identity
toId = mypartition (\x -> x `mod` 2 == 1) . toGads

mypartition p xs = (l, r)
  where
    l = MS.filter (\x -> x `mod` 2 == 1) xs
    r = MS.filter (\x -> even x) xs

tgcg2distr :: TGCG -> Distribution Int
tgcg2distr (tg, cg) = d
  where
    ws = wiresOfGads tg
    sws = length ws
    wsp = [(w, p) | w <- ws, let p = prob w]
    prob w = MS.size (MS.filterWithKey (\k _ -> w `member` k) tg) % sws
    d = fromList wsp

-- | non repeat sampling
sampleNwire :: Int -> Int -> Distribution Int -> [Int]
sampleNwire seed n distr = ws
  where
    gen = fromDistribution distr
    sampleN 0 d g = ([], g)
    sampleN n d g =
      let (w, g') = sample (fromDistribution d) g
       in let (ws, g'') = sampleN (n -1) (assuming (/= w) d) g'
           in (w : ws, g'')
    ws = fst $ sampleN n distr (mkStdGen seed)

-- | repeat sampling, efficient, but sample not follow the the
-- distribution exactly.
sampleNwire' :: Int -> Int -> Distribution Int -> [Int]
sampleNwire' seed n distr = ws
  where
    ws =
      if length (nub ws') /= n
        then sampleNwire' (seed + 1) n distr
        else ws'
    gen = fromDistribution distr
    sampleN 0 d g = ([], g)
    sampleN n d g =
      let (w, g') = sample (fromDistribution d) g
       in let (ws, g'') = sampleN (n -1) d g' in (w : ws, g'')
    ws' = fst $ sampleN n distr (mkStdGen seed)

sampleNw :: StdGen -> Int -> Distribution Int -> ([Int], StdGen)
sampleNw stdg n di = (ws, stdg')
  where
    (ws'', stdg'') = sample_nrep' n stdg di
    (ws, stdg') =
      if length (nub ws'') < n
        then sampleNw stdg'' n di
        else (ws'', stdg'')

sampleNws :: StdGen -> Int -> Int -> Distribution Int -> ([[Int]], StdGen)
sampleNws stdg 0 n di = ([], stdg) where
sampleNws stdg m n di = (xss, stdg')
  where
    (xss'', stdg'') = sampleNws stdg (m -1) n di
    (x, stdg') = sampleNw stdg'' n di
    xss = x : xss''

-- | check equality of identity

-- nt' > 0 &&

-- | apply an Identity if it can reduce T-count, or if it increases
-- the tcount, apply with a probablity. borrowed the idea from
-- simulated annealing.
tryId :: StdGen -> Int -> Identity -> TGCG -> (TGCG, StdGen)
tryId stdg tol id@(tg, cg) mm@(tm, cm) = case nt > dlen - tol of
  True -> ((tm', cm'), stdg) -- stdg'
  False -> (mm, stdg)
  where
    dlen = MS.size tg `div` 2
    --    (tol', stdg') =  randomR (0,tol) stdg
    tol' = tol
    nt' =
      foldl'
        ( \a x ->
            if Data.Maybe.isJust (MS.lookup (fst x) tm)
              then a
              else a + 1
        )
        0
        (take (dlen + tol + 1) (MS.toList tg))
    tgintm = map (\x -> (x, MS.lookup (fst x) tm)) (MS.toList tg)
    (jkeys, nkeys) = partition (\(x, y) -> Data.Maybe.isJust y) tgintm
    nt = length jkeys
    jkeys' = map fst jkeys
    nkeys' = map fst nkeys
    tm1 = foldl' (\ys x -> MS.delete (fst x) ys) tm jkeys'
    tm' = foldl' (\ys x -> uncurry MS.insert x ys) tm1 nkeys'
    cm1 =
      foldl' (\ys x -> MS.insertWith pmod8 (fst x) (snd x) ys) cm
        (map (\(x, y) -> (fst x, pmod8 (snd x) (unJust y))) jkeys)
    cm' = foldl' (\ys x -> uncurry (MS.insertWith pmod8) x ys) cm1 $ MS.toList cg

tryIds :: StdGen -> Int -> [Identity] -> TGCG -> (TGCG, StdGen)
tryIds stdgen tol ids mm =
  foldl'
    (\mmg x -> {-# SCC "TryId-" #-} tryId (snd mmg) tol x (fst mmg))
    (mm, stdgen)
    ids

pmod8 :: Int -> Int -> Int
pmod8 x y = (x + y) `mod` 8

-- | boolean mask
bmask' n g
  | n == 0 = ([], g)
  | otherwise =
    let (a, g') = sample (fromDistribution (uniform [True, False])) g
     in let (as, g'') = bmask' (n - 1) g'
         in (a : as, g'')

bmask seed n = fst $ bmask' n (mkStdGen seed)

id4ton :: Int -> Int -> Identity
id4ton seed n
  | n < 4 = error "id4ton: wires less than 4"
  | otherwise = toId idns
  where
    ws = [0 .. n -1]
    fs = [4 .. n]
    wss = concatMap (\x -> choosen_linear x ws) fs
    bm = bmask seed (length wss -1)
    bmwss = zip wss (bm ++ [True])
    wss' = map fst $ filter (snd) bmwss
    idns = concatMap idnw wss'

id4ton_dis :: Int -> Int -> Int -> Identity
id4ton_dis seed n len
  | n < 4 = error "id4ton: wires less than 4"
  | otherwise = toId idns
  where
    ws = [0 .. n -1]
    fs = [4 .. n]
    wss = concatMap (\x -> choosen_linear x ws) fs
    wss' =
      fst $
        sample_nrep
          len
          (mkStdGen seed)
          (fromList freq)
    tot = sum (map (exp . fromIntegral . length) wss)
    freq = map (\x -> (x, exp (fromIntegral $ length x) / tot)) wss
    idns = concatMap idnw wss'

sample_rep n g gen
  | n == 0 = ([], g)
  | otherwise =
    let (a, g') = sample gen g
     in let (as, g'') = sample_rep (n - 1) g' gen
         in (a : as, g'')

sample_nrep n g dis
  | n == 0 = ([], g)
  | otherwise =
    let (a, g') = sample (fromDistribution dis) g
     in let (as, g'') = sample_nrep (n - 1) g' (assuming (/= a) dis) in (a : as, g'')

sample_nrep' n g dis
  | n == 0 = ([], g)
  | otherwise =
    let (a, g') = sample (fromDistribution dis) g
     in let (as, g'') = sample_nrep (n - 1) g' (assuming (/= a) dis) in (a : as, g'')

expandId :: Identity -> [[Int]] -> [Identity]
expandId id@(tg, cg) wss =
  [ ( MS.fromList (map (Data.Bifunctor.first f) (MS.toList tg)),
      MS.fromList (map (\(k, v) -> (f k, v)) (MS.toList cg))
    )
    | f <- candisfs
  ]
  where
    candisfs :: [Wires -> Wires]
    candisfs = [Set.map f | f <- fs]
    fs :: [Int -> Int]
    fs = [pair2fun (zip idws iws) | iws <- wss, length iws == length idws]
    idws = wiresOfId id

choosen_linear :: Int -> [a] -> [[a]]
choosen_linear 0 xs = []
choosen_linear 1 xs = map (: []) xs
choosen_linear n xs
  | length xs < n = []
  | length xs == n = [xs]
choosen_linear n xs@(h : t) = ys : zss
  where
    ys = take n xs
    zss = choosen_linear n t

pair2fun :: [(Int, Int)] -> (Int -> Int)
pair2fun [] x = x
pair2fun p x = Data.Maybe.fromMaybe x (lookup x p)

wiresOfGads :: Gads -> [Int]
wiresOfGads gads = Set.toList $ foldl' Set.union Set.empty (MS.keys gads)

wiresOfId :: Identity -> [Int]
wiresOfId id@(t, c) = Set.toList $ foldl' Set.union Set.empty (MS.keys t ++ MS.keys c)

-- | return n-gadgets that lies on first (n+4) wires, input gads
-- should have keys of the same size.
ngads :: Int -> Gads -> Gads
ngads n = MS.foldlWithKey'
    ( \b k v ->
        let b' = MS.insert k v b
         in if (length $ wiresOfGads b') <= n + 4
              then b'
              else b
    )
    (MS.empty)

unif :: TGCG -> Distribution Int
unif (tg, cg) = uniform $ wiresOfGads tg

-- | randomlistN g n m generate n distinct random numbers from 0 .. m-1
randomlistN :: StdGen -> Int -> Int -> ([Int], StdGen)
randomlistN sg 0 _ = ([], sg)
randomlistN sg 1 m = ([a], sg')
  where
    (a, sg') = randomR (0, m -1) sg
randomlistN sg n m = (as1, sg1)
  where
    (a, sg') = {-# SCC "randomR-" #-} randomR (0, m -1) sg
    (as, sg'') = randomlistN sg' (n -1) m
    (as1, sg1) =
      if a `elem` as
        then randomlistN sg'' n m
        else (a : as, sg'')

mysampleNws :: StdGen -> Int -> Int -> [Int] -> ([[Int]], StdGen)
mysampleNws sg 0 len ws = ([], sg)
mysampleNws sg n len ws = (xss, sgr)
  where
    (xss', sg') = mysampleNws sg (n -1) len ws
    (x, sg'') = {-# SCC "randomlistN-" #-} randomlistN sg' len m
    m = length ws
    x' = map (ws !!) x
    xss = x' : xss'
    sgr = sg''

runIds_sa :: StdGen -> Int -> Int -> [Identity] -> TGCG -> (TGCG, StdGen)
runIds_sa stdgen rep tol [] mm = (mm, stdgen)
runIds_sa stdgen rep tol (h : t) mm = (mm'', stdgen')
  where
    len = length (wiresOfId h)
    wog = wiresOfGads $ fst mm
    (candis_wires, stdgen'') =
      --    = sampleNws stdgen 50000 len (tgcg2distr mm)
      {-# SCC "mysample-" #-} mysampleNws stdgen rep len wog
    idss = expandId h candis_wires
    (mm', stdg') = {-# SCC "TryIds-" #-} tryIds stdgen'' tol idss mm
    (mm'', stdgen') = runIds_sa stdg' rep tol t mm'

runIds_saw :: StdGen -> Int -> Int -> Int -> [Identity] -> TGCG -> (TGCG, StdGen)
runIds_saw stdgen rep tol woglen [] mm = (mm, stdgen)
runIds_saw stdgen rep tol woglen (h : t) mm = (mm'', stdgen')
  where
    len = length (wiresOfId h)
    wog = [0 .. woglen -1] -- wiresOfGads $ fst mm
    (candis_wires, stdgen'') =
      --    = sampleNws stdgen 50000 len (tgcg2distr mm)
      {-# SCC "mysample-" #-} mysampleNws stdgen rep len wog
    idss = expandId h candis_wires
    (mm', stdg') = {-# SCC "TryIds-" #-} tryIds stdgen'' tol idss mm
    (mm'', stdgen') = runIds_saw stdg' rep tol woglen t mm'

runIds_saf :: StdGen -> Int -> [Identity] -> TGCG -> (TGCG, StdGen)
runIds_saf stdgen tol [] mm = (mm, stdgen)
runIds_saf stdgen tol ids@(h : t) mm = (mm'', stdgen')
  where
    len = length (wiresOfId h)
    wog = wiresOfGads $ fst mm
    candis_wires = F.choosen len wog
    idss = expandId h candis_wires
    (mm', stdg') = {-# SCC "TryIds-" #-} tryIds stdgen tol idss mm
    (mm'', stdgen') = runIds_saf stdg' tol t mm'

runIds_safr :: StdGen -> Int -> [Identity] -> TGCG -> (TGCG, StdGen)
runIds_safr stdgen tol [] mm = (mm, stdgen)
runIds_safr stdgen tol ids mm = (mm', stdg')
  where
    (mm1, stdg1) = runIds_saf stdgen tol ids mm
    (mm', stdg') = if mm1 == mm then (mm, stdg1) else runIds_safr stdg1 tol ids mm1

zxid45s = map toId ids'
  where
    w4s = F.choosen 4 [0 .. 4] ++ F.choosen 5 [0 .. 4]
    w4se = map (\x -> [[], x]) w4s
    w45' =
      [ idnw x1 ++ idnw x2 ++ idnw x3 ++ idnw x4 ++ idnw x5 ++ idnw x6
        | x1 <- head w4se,
          x2 <- w4se !! 1,
          x3 <- w4se !! 2,
          x4 <- w4se !! 3,
          x5 <- w4se !! 4,
          x6 <- w4se !! 5
      ]
    ids' = drop 1 w45'

runIds_r :: StdGen -> Int -> [Identity] -> TGCG -> LMMR
runIds_r stdgen rep ids mm = mm'
  where
    tols = [0] :: [Int]
    rts = map (\ x (a, g) -> {-# SCC "runIds_sa-" #-} runIds_saf g x ids a) tols
    ct = foldl (.) id rts
    ct' = if length wog <= 21 then (\(a, g) -> {-# SCC "runIds_sa-" #-} runIds_safr g 0 ids a) else (\(a, g) -> {-# SCC "runIds_sa-" #-} runIds_sa g rep 0 ids a)
    mm' = return $ fst $ ct' (mm, stdgen)
    wog = wiresOfGads $ fst mm

runIds_rw :: StdGen -> Int -> [Identity] -> TGCG -> LMMR
runIds_rw stdgen rep ids mm = do
  (_, (vq, fq), _) <- get
  let tols = [0] :: [Int]
  let rts = map (\ x (a, g) -> {-# SCC "runIds_sa-" #-} runIds_saf g x ids a) tols
  let ct = foldl (.) id rts
  let ct' = if fq <= 27 then (\(a, g) -> {-# SCC "runIds_sa-" #-} runIds_safr g 0 ids a) else (\(a, g) -> {-# SCC "runIds_sa-" #-} runIds_saw g rep 0 fq ids a)
  let mm' = fst $ ct' (mm, stdgen)
  return mm'

class ToCir a where
  tocir :: a -> [Gate]
  tocir' :: a -> [Gate]

instance ToCir LMR where
  tocir lmr = cir
    where
      (a, (s', vqfq, tct)) = runState lmr (([], []), (0, 0), (0, 0, 0))
      s = s'
      a' = zx2cir a
      cir = zx2cir $ fst s ++ a ++ snd s

  tocir' lmr = cir
    where
      (a, (s', vqfq, tct)) = runState lmr (([], []), (0, 0), (0, 0, 0))
      s = s'
      a' = zx2cir a
      cir = fst s ++ a ++ snd s

instance F.PDFable LMR where
  topdf lmr = F.topdf (F.LMR l m r)
    where
      (a, (s', vqfq, tct)) = runState lmr (([], []), (0, 0), (0, 0, 0))
      s = s'
      l = fst s
      m = a
      r = snd s
  topdf_file lmr = F.topdf_file (F.LMR l m r)
    where
      (a, (s', vqfq, tct)) = runState lmr (([], []), (0, 0), (0, 0, 0))
      s = s'
      l = fst s
      m = a
      r = snd s

gads2zx :: Gads -> [F.ZXAtom]
gads2zx gs = map (\(k, v) -> F.G v (Set.toList k)) $ MS.toList gs

mm2zx :: TGCG -> [F.ZXAtom]
mm2zx (tm, cm) = gads2zx tm ++ gads2zx cm

mycmp (Ga p1 ws1) (Ga p2 ws2)
  | Set.size ws1 == Set.size ws2 = compare ws1 ws2
  | otherwise = compare (Set.size ws1) (Set.size ws2)

instance F.PDFable LMMR where
  topdf lmmr = F.topdf (F.LMR l m r)
    where
      (a, (s', vqfq, tct)) = runState lmmr (([], []), (0, 0), (0, 0, 0))
      s = s'
      l = fst s
      m = sortBy mycmp (gads2cir' (MS.toList $ fst a)) ++ gads2cir' (MS.toList $ snd a)
      r = snd s

{-  topdf_file lmmr = F.topdf_file (F.LMR l m r) where
    (a,s') = runState lmmr (([],[]),0)
    s = fst s'
    l =  (F.cir2zx $ fst s)
    m =  mm2zx a
    r =  (F.cir2zx $ snd s)
-}

instance ToCir LMMR where
  tocir lmr = cir
    where
      (a, (s', vqfq, tct)) = runState lmr (([], []), (0, 0), (0, 0, 0))
      s = s'
      a' = gads2cir (MS.toList (fst a) ++ MS.toList (snd a))
      cir = zx2cir $ fst s ++ a' ++ snd s
  tocir' lmr = cir
    where
      (a, (s', vqfq, tct)) = runState lmr (([], []), (0, 0), (0, 0, 0))
      s = s'
      a' =
        gads2cir' (MS.toList (fst a))
          ++ gads2cir' (MS.toList (snd a))
      cir = fst s ++ a' ++ snd s

p2zst :: Int -> Int -> [Gate]
p2zst 1 j = [T j]
p2zst 3 j = [T j, S j]
p2zst 5 j = [T j, Z j]
p2zst 7 j = [T j, Z j, S j]
p2zst 2 j = [S j]
p2zst 4 j = [Z j]
p2zst 6 j = [Z j, S j]
p2zst 0 j = []

stepcnot :: [Int] -> [Gate]
stepcnot [a, b] = [Cnot b a]
stepcnot (a : b : c : t) = Cnot (last (c : t)) a : stepcnot (b : c : t)

gad2cir' :: Gadget -> [Gate]
gad2cir' (ws, p) = [Ga p ws]

gads2cir' :: [Gadget] -> [Gate]
gads2cir' xs = sortBy mycmp $ concatMap gad2cir' xs

gad2cir :: Gadget -> [Gate]
gad2cir (ws, i') = case len of
  1 -> p2zst i (Set.elemAt (len - 1) ws)
  0 -> []
  _ -> casl ++ sts ++ casr
  where
    i = i' `mod` 8
    ws' = Set.toList ws
    len = Set.size ws
    casl = stepcnot ws'
    casr = reverse casl
    sts = p2zst i (Set.elemAt (len - 1) ws)

gads2cir :: [Gadget] -> [Gate]
gads2cir xs = concatMap gad2cir xs

gads2cir2 xs = gads2cir (MS.toList xs)

zx2cir1 :: Gate -> [Gate]
zx2cir1 (Ga p ws) = gad2cir (ws, p)
zx2cir1 x = [x]

zx2cir cir = concatMap zx2cir1 cir

g2zx :: Gate -> [Gate]
g2zx (CCZ i j k) = ccz_to_7gs i j k
g2zx (CZ i j) = [Ga 6 (sfl [i, j]), Ga 2 (sfl [i]), Ga 2 (sfl [j])]
g2zx (Z i) = [Ga 4 (sfl [i])]
g2zx (S i) = [Ga 2 (sfl [i])]
g2zx (T i) = [Ga 1 (sfl [i])]
g2zx (Cnot i j) = [CX i j]
g2zx (CX i j) = [CX i j]
g2zx (Toffoli i j k) = [H i] ++ g2zx (CCZ i j k) ++ [H i]
g2zx (CCX i j k) = [H i] ++ g2zx (CCZ i j k) ++ [H i]
g2zx (Swap i j) = [Swap i j]
g2zx (X i) = [X i]
g2zx (H i) = [H i]
g2zx (Init s i) = [Init s i]
g2zx (Term s i) = []

cir2zx cir = concatMap g2zx cir

cir2lxr :: [Gate] -> LMR
cir2lxr cir = return $ cir2zx cir

initLMR :: [Gate] -> LMR
initLMR cir = do
  let lws = length $ wiresOfCir cir
  put (([], []), (lws, lws), (0, 0, 0))
  return (map C.cnot2cx cir)

initLMR' :: Int -> [Gate] -> LMR
initLMR' vq cir = do
  let lws = vq
  put (([], []), (lws, lws), (0, 0, 0))
  return (map C.cnot2cx cir)

isTGadget :: Gadget -> Bool
isTGadget (ws, p) = p `mod` 2 == 1

isTGad :: Gate -> Bool
isTGad (Ga p ws) = isTGadget (ws, p)
isTGad _ = False

-- | work only when Gadget in the middle
tolmmr' :: [Gadget] -> LMMR
tolmmr' gs = return $ toId gs

tolmmr :: [Gate] -> LMMR
tolmmr zx = do
  ((l, r), (vq, fq), (int, fut, idt)) <- get
  let gads = toId $ map (\(Ga p ws) -> (ws, p)) zx
  let int' = length $ filter isTGad zx
  let fut' = MS.size (fst gads)
  put ((l, r), (vq, fq), (int', fut', idt))
  return gads

-- | CCZ to 7 gadgets
ccz_to_7gs i j k = [Ga 1 (sfl [i]), Ga 1 (sfl [j]), Ga 1 (sfl [k]), Ga 7 (sfl [i, j]), Ga 7 (sfl [i, k]), Ga 7 (sfl [j, k]), Ga 1 (sfl [i, j, k])]

-- | pick out the Clifford gates on the left and right ends, put them
-- in LMR.
pickClifford :: [Gate] -> LMR
pickClifford cir = do
  ((l, r), (vq, fq), tct) <- get
  let cir1 = dropWhile isCliffordg cir
  let cl = takeWhile isCliffordg cir
  let cir2 = dropWhileB isCliffordg cir1
  let cr = takeWhileB isCliffordg cir1
  let cr' = cr ++ r
  let cl' = l ++ cl
  put ((cl', cr'), (vq, fq), tct)
  return cir2

moveH_old :: [Gate] -> LMR
moveH_old cir = do
  let circx = map C.cnot2cx cir
  cirm <- pickClifford circx
  ((l, r), (vq, fq), tct) <- get
  let F.LMR aa bb cc = F.moveh (map C.cx2cnot cirm)
  let cr' = cc ++ r
  let cl' = l ++ aa
  put ((cl', cr'), (vq, fq), tct)
  return bb

moveH :: [Gate] -> LMR
moveH cir = do
  let circx = map C.cnot2cx cir
  cirm <- pickClifford circx
  ((l, r), (vq, fq), tct) <- get
  let (p1, p2) = S.mvh cirm
  let cr' = p2 ++ r
  let (pi1, pi2) = S.mvh $ inv_cir p1
  let cir2 = inv_cir pi1
  let cl = inv_cir pi2
  let cl' = l ++ cl
  put ((cl', cr'), (vq, fq), tct)
  return cir2

moveCXp :: [Gate] -> LMR
moveCXp cir = do
  let circx = map C.cnot2cx cir
  cirm <- pickClifford circx
  ((l, r), (vq, fq), tct) <- get
  let (p1, p2) = C.mvcx cirm
  let cr' = p2 ++ r
  let (pi1, pi2) = C.mvcx $ inv_cir p1
  let cir2 = inv_cir pi1
  let cl = inv_cir pi2
  let cl' = l ++ cl
  put ((cl', cr'), (vq, fq), tct)
  return cir2

type LR' a = State ([Gate], [Gate]) a

type MR a = State [Gate] a

myput x xs = do
  r <- get
  put $ x : r
  return xs

insertcsg :: Gate -> [Gate] -> MR [Gate]
insertcsg (Swap i j) [] = myput (Swap i j) []
insertcsg (CX i j) [] = myput (CX i j) []
insertcsg (Ga p ws) []
  | Set.size ws > maxW = myput (Ga p ws) []
  | otherwise = return [Ga p ws]
insertcsg (Swap i j) (CX k l : t)
  | (length . nub) [i, j, k, l] == 2 = do
    t' <- insertcsg (Swap i j) t
    return $ CX k l : t'
  | (length . nub) [i, j, k, l] == 4 = do
    t' <- insertcsg (Swap i j) t
    return $ CX k l : t'
  | (length . nub) [i, j, k, l] == 3 = do
    t' <- insertcsg (Swap i j) t
    return $ CX k' l' : t'
  where
    (k', l') = if i == k then (j, l) else (case i == l of
                                    True -> (k, j)
                                    False -> case j == k of
                                      True -> (i, l)
                                      False -> (k, i))
insertcsg (Swap i j) (Ga p ws : t) = do
  t' <- insertcsg (Swap i j) t
  return $ x' : t'
  where
    x' = Ga p ws'
    ws' = case (i `member` ws, j `member` ws) of
      (True, True) -> ws
      (False, True) -> Set.insert i $ Set.delete j ws
      (True, False) -> Set.insert j $ Set.delete i ws
      _ -> Set.insert j $ Set.delete i ws
insertcsg a@(CX i j) (b@(CX k l) : t)
  | (length . nub) [i, j, k, l] == 2 && i == k = return t
  | (length . nub) [i, j, k, l] == 2 && i == l =
    insertcsg (CX i j) t >>= insertcsg (Swap i j)
  | i == l || j == k = return $ a : b : t
  | otherwise = do
    t' <- insertcsg (CX i j) t
    if head t' /= CX i j
      then insertcsg (CX k l) t'
      else return $ a : b : t
insertcsg (CX i j) (Ga p ws : t)
  | not (i `member` ws) = do
    t' <- insertcsg (CX i j) t
    insertcsg (Ga p ws) t'
  | j `member` ws = do
    t' <- insertcsg (CX i j) t
    insertcsg (Ga p (Set.delete j ws)) t'
  | Set.size ws < maxW = do
    t' <- insertcsg (CX i j) t
    insertcsg (Ga p (Set.insert j ws)) t'
  | otherwise = return $ CX i j : Ga p ws : t
insertcsg a@(Ga p ws) (b@(Ga p' ws') : t)
  | a > b = do
    t' <- insertcsg a t
    insertcsg b t'
  | otherwise = return $ a : b : t
insertcsg a (b : t) = return $ a : b : t

movecxr :: [Gate] -> ([Gate], [Gate])
movecxr xs = (x, y)
  where
    (x, y) = runState (normalizecsg xs) []

normalizecsg :: [Gate] -> MR [Gate]
normalizecsg zx = insertcsgs zx []

-- | do we really need "reverse xs"
insertcsgs xs ys = foldM (flip insertcsg) ys (reverse xs)

insertcs :: Gate -> [Gate] -> [Gate]
insertcs (Swap i j) [] = [Swap i j]
insertcs (CX i j) [] = [CX i j]
insertcs (Swap i j) (Swap k l : t)
  | (length . nub) [i, j, k, l] == 2 = t
  | Swap i j > Swap k l = insertcs (Swap k l) $ insertcs (Swap i' j') t
  | Swap i j <= Swap k l = Swap i j : (Swap k l : t)
  where
    i' = if i == k then l else (case i == l of
                         True -> k
                         _ -> i)
    j' = if j == k then l else (case j == l of
                         True -> k
                         _ -> j)
insertcs (Swap i j) (CX k l : t)
  | (length . nub) [i, j, k, l] == 2 = insertcs (CX l k) $ insertcs (Swap i j) t
  | otherwise = insertcs (CX k' l') $ insertcs (Swap i j) t
  where
    k' = if k == i then j else (case k == j of
                         True -> i
                         _ -> k)
    l' = if l == i then j else (case l == j of
                         True -> i
                         _ -> l)
insertcs (CX i j) (Swap k l : t) = CX i j : (Swap k l : t)
insertcs a@(CX i j) (b@(CX k l) : t)
  | (length . nub) [i, j, k, l] == 2 && i == k = t
  | (length . nub) [i, j, k, l] == 2 && i == l =
    insertcs (Swap i j) $ insertcs (CX i j) t
  | not (i == l || j == k) && a > b =
    insertcs (CX k l) $ insertcs (CX i j) t
  | (i == l || j == k) && a > b =
    insertcs b $ insertcs (CX i' j') $ insertcs a t
  | a <= b = a : (b : t)
  where
    i' = if i == l then k else i
    j' = if j == k then l else j

normalizecs :: [Gate] -> [Gate]
normalizecs zx = insertcss zx []

insertcss xs ys = foldl' (flip insertcs) ys (reverse xs)

moveswap_step :: [Gate] -> LR' [Gate]
moveswap_step [a]
  | isCliffordg a = do
    (l, r) <- get
    put (l, a : r)
    return []
  | otherwise = error "moveswap_step: [a] is not [Swap]"
moveswap_step (Swap i j : Ga p ws : t)
  | i `member` ws && j `member` ws =
    return $ Ga p ws : Swap i j : t
  | i `member` ws = return $ Ga p wsj : Swap i j : t
  | j `member` ws = return $ Ga p wsi : Swap i j : t
  | otherwise = return $ Ga p ws : Swap i j : t
  where
    wsj = Set.insert j $ Set.delete i ws
    wsi = Set.insert i $ Set.delete j ws
moveswap_step (Swap i j : Swap k l : t) = do
  t' <- moveswap_step $ Swap k l : t
  moveswap_step $ Swap i j : t'
moveswap_step (Swap i j : CX k l : t)
  | (i == k && j == l) || (i == l && j == k) =
    return $ CX l k : Swap i j : t
  | i == k && j /= l = return $ CX j l : Swap i j : t
  | i /= k && j == l = return $ CX k i : Swap i j : t
  | i == l && j /= k = return $ CX k j : Swap i j : t
  | i /= l && j == k = return $ CX i l : Swap i j : t
  | otherwise = return $ CX k l : Swap i j : t

moveswap :: [Gate] -> LR' [Gate]
moveswap [] = return []
moveswap [a] = do
  (l, r) <- get
  if isCliffordg a then put (l, a : r) else put (a : l, r)
  return []
moveswap (Swap i j : t) = do
  t' <- moveswap_step $ Swap i j : t
  moveswap t'
moveswap (h : t) = do
  (l, r) <- get
  put (h : l, r)
  moveswap t

moveswap' :: [Gate] -> ([Gate], [Gate])
moveswap' zx = (m, r)
  where
    (m', r) = execState (moveswap zx) ([], [])
    m = reverse m'

fusion_wcx1 :: Gate -> [Gate] -> [Gate]
fusion_wcx1 g [] = [g]
fusion_wcx1 (Ga p1 ws1) ((Ga p2 ws2) : t)
  | ws1 == ws2 =
    let p3 = ((p1 + p2) `mod` 8)
     in if p3 == 0 then t else Ga p3 ws1 : t
  | ws1 > ws2 = Ga p2 ws2 : fusion_wcx1 (Ga p1 ws1) t
fusion_wcx1 (CX i j) ((Ga p ws) : t)
  | i `member` ws && j `member` ws =
    Ga p (Set.delete j ws) : fusion_wcx1 (CX i j) t
  | i `member` ws && not (j `member` ws) =
    Ga p (Set.insert j ws) : fusion_wcx1 (CX i j) t
  | otherwise = Ga p ws : fusion_wcx1 (CX i j) t
fusion_wcx1 h t = h : t

fusion_wcx xs ys = foldl' (flip fusion_wcx1) ys (reverse xs)

fusion xs = fusion_wcx xs []

fusion_wCX :: [Gate] -> LMR
fusion_wCX xs = return $ fusion xs

movecx_R = repeatedly movecx_r

movecx_r :: [Gate] -> Maybe [Gate]
movecx_r [] = Nothing
movecx_r (CX i j : Ga p ws : t)
  | i `member` ws && j `member` ws = do
    return $ Ga p wsi : CX i j : t
  | i `member` ws = do
    return $ Ga p wsj : CX i j : t
  | otherwise = do
    return $ Ga p ws : CX i j : t
  where
    wsj = Set.insert j ws
    wsi = Set.delete j ws
movecx_r (CX i j : CX k l : t) = do
  t' <- movecx_r $ CX k l : t
  movecx_r $ CX i j : t'
movecx_r (h : t) = do
  t' <- movecx_r t
  return $ h : t'

movecx_Left xs = (reverse lcx, reverse lg)
  where
    (lcx, lg) = execState (mvcx_left xs) ([], [])

mymvcx :: [Gate] -> ([Gate], [Gate])
mymvcx [] = ([], [])
mymvcx (CX i j : t) = (tg', CX i j : tcx)
  where
    (tg, tcx) = mymvcx t
    tg' = movecx_left' (CX i j) tg
mymvcx (h : t) = (h : tg, tcx)
  where
    (tg, tcx) = mymvcx t

-- | tail recursive
mvcxtc :: [Gate] -> ([Gate], [Gate]) -> ([Gate], [Gate])
mvcxtc [] (mg, rcx) = (mg, rcx)
mvcxtc (CX i j : t) (mg, rcx) = mvcxtc t (mg', CX i j : rcx)
  where
    mg' = movecx_tc (CX i j) mg []
mvcxtc (h : t) (mg, rcx) = mvcxtc t (h : mg, rcx)

mvcxTC xs = mvcxtc xs ([], [])

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x : xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

mvcx_left = foldM' movecx_left2 ()

movecx_left2 :: () -> Gate -> LR' ()
movecx_left2 () (CX i j) = do
  (lcx, lg) <- get
  let lg' = movecx_left' (CX i j) lg
  put (CX i j : lcx, lg')
movecx_left2 () h = do
  (lcx, lg) <- get
  put (lcx, h : lg)

movecx_left :: [Gate] -> LR' [Gate]
movecx_left xs@(CX i j : t) = do
  (lcx, lg) <- get
  let lg' = movecx_left' (CX i j) lg
  put (CX i j : lcx, lg')
  movecx_left t
movecx_left xs@(h : t) = do
  (lcx, lg) <- get
  put (lcx, h : lg)
  movecx_left t
movecx_left [] = return []

movecx_left' (CX i j) [] = []
movecx_left' (CX i j) (Ga p ws : tt) = Ga p ws' : movecx_left' (CX i j) tt
  where
    ws' = if i `member` ws then (case j `member` ws of
                           True -> Set.delete j ws
                           False -> Set.insert j ws) else ws

movecx_tc (CX i j) [] cs = cs
movecx_tc (CX i j) (Ga p ws : tt) cs = movecx_tc (CX i j) tt (Ga p ws' : cs)
  where
    ws' = if i `member` ws then (case j `member` ws of
                           True -> Set.delete j ws
                           False -> Set.insert j ws) else ws

movecxccx_i :: [Gate] -> LR' ([Gate], Int)
movecxccx_i [a]
  | isCliffordg a = do
    (l, r) <- get
    put (l, a : r)
    return ([], 0)
movecxccx_i (a@(CX i j) : b@(CCX k l m) : t)
  | j == k = do
    (t', n) <- movecxccx_i $ a : t
    return (b : CCX i l m : t', n + 1)
  | otherwise = do
    (t', n) <- movecxccx_i $ a : t
    return (b : t', n)
movecxccx_i (a@(CX i j) : b@(CX k l) : t)
  | i == k && j == l = return (t, 0)
  | i == l && j == k = do
    (t', n') <- movecxccx_i $ a : t
    let (t'', sw) = moveswap' (Swap i j : t')
    (l, r) <- get
    put (l, sw ++ r)
    return (t'', n')
  | i == l && j /= k = do
    (t', n') <- movecxccx_i $ b : t
    (t'', n'') <- movecxccx_i $ a : t'
    return $ (t'', n'' + n')
  | i /= l && j == k = do
    (t', n') <- movecxccx_i $ b : t
    (t'', n'') <- movecxccx_i $ a : t'
    return $ (t'', n'' + n')
  | otherwise = do
    (t', n') <- movecxccx_i $ a : t
    return (b : t', n')

movecxccx_i' :: [Gate] -> (([Gate], Int), ([Gate], [Gate]))
movecxccx_i' zx = runState (movecxccx_i zx) ([], [])

movecxccx :: ([Gate], [Gate]) -> LR' ([Gate], [Gate])
movecxccx (m, a@(CX i j) : t) = do
  let ((tr, nr), (lr, rr)) = movecxccx_i' $ a : t
  let ((tl, nl), (ll, rl)) = movecxccx_i' $ a : m
  case nr < nl of
    True -> do
      (l, r) <- get
      put (l, rr ++ r)
      movecxccx (m, tr)
    -- insertcss rr r)
    --      seq (movecxccx (m,tr)) (movecxccx (m,tr))
    False -> do
      (l, r) <- get
      put (l ++ reverse rl, r)
      movecxccx (tl, t)
--      seq (movecxccx (tl, t)) (movecxccx (tl, t))
movecxccx (m, h : t) = do
  movecxccx (h : m, t)
movecxccx (m, []) = return (m, [])

movecxccx' :: [Gate] -> ([Gate], [Gate], [Gate])
movecxccx' zx = (l, m, r)
  where
    ((ml, mr), (l, r)) = runState (movecxccx ([], zx)) ([], [])
    m = reverse ml

movecx_i :: [Gate] -> LR' ([Gate], Int)
movecx_i [a]
  | isCliffordg a = do
    (l, r) <- get
    put (l, a : r)
    return ([], 0)
movecx_i (a@(CX i j) : b@(Ga p ws) : t)
  | i `member` ws && j `member` ws = do
    (t', n') <- movecx_i $ a : t
    return
      ( Ga p wsi : t',
        if p `mod` 2 == 1 then n' - (Set.size ws) else n'
      )
  | i `member` ws = do
    (t', n') <- movecx_i $ a : t
    return
      ( Ga p wsj : t',
        if p `mod` 2 == 1 then n' + (Set.size ws) else n'
      )
  | otherwise = do
    (t', n') <- movecx_i $ a : t
    return (b : t', n')
  where
    wsj = Set.insert j ws
    wsi = Set.delete j ws
movecx_i (a@(CX i j) : b@(CX k l) : t)
  | i == k && j == l = return (t, 0)
  | i == l && j == k = do
    (t', n') <- movecx_i $ a : t
    let (t'', sw) = moveswap' (Swap i j : t')
    (l, r) <- get
    put (l, sw ++ r)
    return (t'', n')
  | i == l && j /= k = do
    (t', n') <- movecx_i $ b : t
    (t'', n'') <- movecx_i $ a : t'
    return $ (t'', n'' + n')
  | i /= l && j == k = do
    (t', n') <- movecx_i $ b : t
    (t'', n'') <- movecx_i $ a : t'
    return $ (t'', n'' + n')

  {-  | (i == l && j /= k) = do
         (t', n') <- movecx_i $ CX k j : t
         (t'', n'') <- movecx_i $ CX i j : t'
         return $ (CX k l : t'', n'' + n')
    | (i /= l && j == k) = do
         (t', n') <- movecx_i $ CX i l : t
         (t'', n'') <- movecx_i $ CX i j : t'
         return $ (CX k l : t'', n'' + n')
  -}
  | otherwise = do
    (t', n') <- movecx_i $ a : t
    return (b : t', n')

movecx_i' :: [Gate] -> (([Gate], Int), ([Gate], [Gate]))
movecx_i' zx = runState (movecx_i zx) ([], [])

myfilt :: Int -> Gate -> Bool
myfilt i (Ga p ws) = i `member` ws && Set.size ws <= 6
myfilt i _ = False

movecx :: ([Gate], [Gate]) -> LR' ([Gate], [Gate])
movecx (m, a@(CX i j) : t) = do
  let ((tr, nr), (lr, rr)) = movecx_i' $ a : t
  let tt' = filter (not . isCliffordg) t
  let nrt = filter (myfilt i) t
  let nrc = filter (\(Ga p ws) -> j `member` ws) nrt
  let nr' = length nrt - 2 * length nrc
  let nlt = filter (myfilt i) m
  let nlc = filter (\(Ga p ws) -> j `member` ws) nlt
  let nl' = length nlt - 2 * length nlc
  let ((tl, nl), (ll, rl)) = movecx_i' $ a : m
  case nr' < nl' of
    True -> do
      (l, r) <- get
      put (l, rr ++ r)
      movecx (m, tr)
    -- insertcss rr r)
    --      seq (movecx (m,tr)) (movecx (m,tr))
    False -> do
      (l, r) <- get
      put (l ++ reverse rl, r)
      movecx (tl, t)
--      seq (movecx (tl, t)) (movecx (tl, t))
movecx (m, h : t) = do
  movecx (h : m, t)
movecx (m, []) = return (m, [])

movecx' :: [Gate] -> ([Gate], [Gate], [Gate])
movecx' zx = (l, m, r)
  where
    ((ml, mr), (l, r)) = runState (movecx ([], zx)) ([], [])
    m = reverse ml

member :: (Ord a => a -> Set.Set a -> Bool)
member = Set.member

--type LR a = StateT (([Gate],[Gate])) Maybe a

movex_step :: [Gate] -> LR' [Gate]
movex_step [a]
  | isCliffordg a = do
    (l, r) <- get
    put (l, a : r)
    return []
movex_step (X i : Ga p ws : t)
  | i `Set.member` ws = return $ Ga (- p `mod` 8) ws : X i : t
  | otherwise = return $ Ga p ws : X i : t
movex_step (X i : X j : t)
  | i == j = return t
  | otherwise = do
    t' <- movex_step $ X i : t
    movex_step $ X j : t'
movex_step (X i : CX j k : t)
  | i == k = return $ CX j k : X j : X k : t
  | otherwise = return $ CX j k : X i : t
movex_step (X i : Swap j k : t)
  | i == j = return $ Swap j k : X k : t
  | i == k = return $ Swap j k : X j : t
  | otherwise = return $ Swap j k : X i : t

maxW = 8

movecx_step :: [Gate] -> LR' [Gate]
movecx_step [a]
  | isCliffordg a || lws > maxW = do
    (l, r) <- get
    put (l, a : r)
    return []
  where
    lws = length $ wiresOfGate a
movecx_step (CX i j : Ga p ws : t)
  | i `member` ws && j `member` ws && lws <= maxW =
    return $ Ga p ws' : CX i j : t
  | i `member` ws && lws < maxW =
    return $ Ga p ws'' : CX i j : t
  | otherwise = return $ Ga p ws : CX i j : t
  where
    lws = Set.size ws
    ws' = Set.delete j ws
    ws'' = Set.insert j ws
movecx_step (CX i j : CX k l : t)
  | i == k && j == l = return t
  | i == l && j == k = do
    t' <- movecx_step $ CX i j : t
    moveswap t'

movex :: [Gate] -> LR' [Gate]
movex [] = return []
movex [a] = do
  (l, r) <- get
  if isCliffordg a then put (l, a : r) else put (a : l, r)
  return []
movex (X i : t) = do
  t' <- movex_step $ X i : t
  movex t'
movex (h : t) = do
  (l, r) <- get
  put (h : l, r)
  movex t

movex' :: [Gate] -> ([Gate], [Gate])
movex' zx = (m, r)
  where
    (m', r) = execState (movex zx) ([], [])
    m = reverse m'

-- | determine whether a Gate is Clifford
isCliffordg :: Gate -> Bool
isCliffordg (H _) = True
isCliffordg (S _) = True
isCliffordg (Z _) = True
isCliffordg (CZ _ _) = True
isCliffordg (X _) = True
isCliffordg (CX _ _) = True
isCliffordg (Swap _ _) = True
isCliffordg (Y _) = True
--isCliffordg (S' _) = True
isCliffordg (Cnot _ _) = True
isCliffordg _ = False

-- | cxccx
cir2lmmr_cxccx cir = initLMR cir >>= moveCXCCX_half_squee >>= (moveH_old . F.desugar_cir) >>= decompH >>= cir2lxr >>= pickClifford >>= moveX >>= moveSwap >>= (\x -> {-# SCC "moveCX'-" #-} moveCX' x) >>= tolmmr

--cir2lmmr :: [Gate] -> LMMR
cir2lmmr cir = initLMR (F.desugar_cir cir) >>= moveH >>= decompH >>= cir2lxr >>= moveSwap >>= moveX >>= moveCX'

cir2lmmr' cir = moveH (F.desugar_cir cir) >>= decompH >>= cir2lxr >>= pickClifford >>= moveX >>= moveSwap >>= (\x -> {-# SCC "moveCX'-" #-} moveCX' x) >>= tolmmr

cir2lmmr'2 cir = moveH cir >>= decompH >>= cir2lxr >>= pickClifford >>= moveX >>= moveSwap >>= (tolmmr . filter (not . isCX))

gf_split :: [Gate] -> ([Gate], [Gate], [Gate])
gf_split cir = (l, m, r)
  where
    l = takeWhile (not . isCX) cir
    m = takeWhile isCX cir
    r = drop (length l + length m) cir
    isCX (CX _ _) = True
    isCX _ = False

isCX (CX _ _) = True
isCX _ = False

{-
gf_opt :: [Gate] -> [Gate]
gf_opt cir = cir' where
  (l,m,r) = gf_split $ tocir' (cir2lmmr'2 cir)
  l' = tolmmr l >>= t
-}

cir2lmmr'' cir = initLMR (F.desugar_cir cir) >>= moveH >>= decompH >>= cir2lxr >>= pickClifford >>= moveX >>= moveSwap >>= fusion_wCX >>= moveCX >>= tolmmr -- >>= runIds id4s

cir2lmmr2 cir = initLMR (F.desugar_cir cir) >>= moveH >>= decompH >>= cir2lxr >>= pickClifford >>= moveX >>= moveSwap >>= fusion_wCX >>= moveCX >>= decompGad 8 >>= tolmmr --  >>= runId 1

tolxr cir = initLMR (F.desugar_cir cir) >>= moveH

tcount :: LMMR -> Int
tcount lmmr = MS.size $ fst $ evalState lmmr (([], []), (0, 0), (0, 0, 0))

tcount_zx :: LMR -> Int
tcount_zx xs = length $ filter isTGad xs'
  where
    xs' = evalState xs (([], []), (0, 0), (0, 0, 0))

tct :: TGCG -> LMMR
tct tgcg = do
  ((l, r), (vq, fq), (int, fut, idt)) <- get
  put ((l, r), (vq, fq), (int, fut, MS.size (fst tgcg)))
  return tgcg

moveSwap :: [Gate] -> LMR
moveSwap zx = do
  let (p1, p2) = moveswap' zx
  ((l, r), (vq, fq), tct) <- get
  put ((l, p2 ++ r), (vq, fq), tct)
  return p1

moveX :: [Gate] -> LMR
moveX zx = do
  let (p1, p2) = movex' zx
  ((l, r), (vq, fq), tct) <- get
  put ((l, p2 ++ r), (vq, fq), tct)
  return p1

-- {-# SCC "moveCX'-" #-}
moveCX' :: [Gate] -> LMR
moveCX' zx = do
  let (cxl, mm, cxr) = movecx' zx
  --  let (mm, cxr) = mvcxTC zx
  --  let cxl = []
  --  let (mm, cxr) = partition (not.isCliffordg) $ movecx_R zx
  ((l, r), (vq, fq), tct) <- get
  put ((l ++ cxl, cxr ++ r), (vq, fq), tct)
  --  put ((l,cxr++r),ii)
  return mm

-- {-# SCC "moveCX'-" #-}
moveCXCCX_cp :: [Gate] -> LMR
moveCXCCX_cp zx = do
  let (cxl, mm, cxr) = movecxccx' zx
  --  let (mm, cxr) = mvcxTC zx
  --  let cxl = []
  --  let (mm, cxr) = partition (not.isCliffordg) $ movecx_R zx
  ((l, r), (vq, fq), tct) <- get
  put ((l ++ cxl, cxr ++ r), (vq, fq), tct)
  --  put ((l,cxr++r),ii)
  return mm

moveCXCCX :: [Gate] -> LMR
moveCXCCX zx = do
  let (xl, xr) = S.halve_cxccx zx
  let (mr, cxr) = S.mv_cxccx xr
  let (ml', cxl') = S.mv_cxccx (reverse xl)
  let ml = reverse ml'
  let cxl = reverse cxl'
  let mm = ml ++ mr
  --  let (mm, cxr) = mvcxTC zx
  --  let cxl = []
  --  let (mm, cxr) = partition (not.isCliffordg) $ movecx_R zx
  ((l, r), (vq, fq), tct) <- get
  put ((l ++ cxl, cxr ++ r), (vq, fq), tct)
  --  put ((l,cxr++r),ii)
  return mm

moveCXCCX_half :: [Gate] -> LMR
moveCXCCX_half zx = do
  let (xl, xr) = S1.halve_cxccx zx
  let (mr, cxr) = C.mvcxccx xr
  let (ml', cxl') = C.mvcxccx (reverse xl)
  let ml = reverse ml'
  let cxl = reverse cxl'
  let mm = ml ++ mr
  --  let (mm, cxr) = mvcxTC zx
  --  let cxl = []
  --  let (mm, cxr) = partition (not.isCliffordg) $ movecx_R zx
  ((l, r), (vq, fq), tct) <- get
  put ((l ++ cxl, cxr ++ r), (vq, fq), tct)
  --  put ((l,cxr++r),ii)
  return mm

moveCXCCX_half_squee :: [Gate] -> LMR
moveCXCCX_half_squee zx = do
  let (xl, xr) = S1.halve_cxccx zx
  let (mr, cxr) = S.mvcxccx xr
  let (ml', cxl') = S.mvcxccx (reverse xl)
  let ml = reverse ml'
  let cxl = reverse cxl'
  let mm = ml ++ mr
  --  let (mm, cxr) = mvcxTC zx
  --  let cxl = []
  --  let (mm, cxr) = partition (not.isCliffordg) $ movecx_R zx
  ((l, r), (vq, fq), tct) <- get
  put ((l ++ cxl, cxr ++ r), (vq, fq), tct)
  --  put ((l,cxr++r),ii)
  return mm

moveCX :: [Gate] -> LMR
moveCX zx = do
  let (mm, xr) = movecxr zx
  let (mm', xr') = movecxr $ reverse mm
  --  let (mm, cxr) = partition (not.isCliffordg) $ movecx_R zx
  ((l, r), (vq, fq), tct) <- get
  put ((l ++ reverse xr', xr ++ r), (vq, fq), tct)
  --  put ((l,cxr++r),ii)
  return $ reverse mm'

takeWhileB :: (a -> Bool) -> [a] -> [a]
takeWhileB p xs = reverse $ takeWhile p $ reverse xs

dropWhileB :: (a -> Bool) -> [a] -> [a]
dropWhileB p xs = reverse $ dropWhile p $ reverse xs

-- | translate HGs to BGS. g2h stands for gate to gadget.
hdecomp :: Gate -> LMR
hdecomp (H i) = do
  ((lo, ro), (vq, fq), tct) <- get
  let l = [Init QMY fq]
  let r = [Term QMY fq]
  put ((lo ++ l, r ++ ro), (vq, fq + 1), tct)
  return [(Swap i fq), (CZ i fq)]
hdecomp g = return [g]

hsdecomp :: [Gate] -> LMR
hsdecomp cir = do
  xss <- mapM hdecomp cir
  return $ concat xss

decompH :: [Gate] -> LMR
decompH = hsdecomp

sfl :: [Int] -> Set.Set Int
sfl = Set.fromList

tc = [H 1, Z 2, X 3, S 4, T 5, Cnot 1 2, CZ 2 3, CCZ 3 4 5]

-- ----------------------------------------------------------------------

-- * Useful general-purpose functions

-- | Repeatedly apply a one-step reduction until it's done.
repeatedly :: (a -> Maybe a) -> (a -> a)
repeatedly f a = case f a of
  Nothing -> a
  Just b -> repeatedly f b

unJust :: Maybe a -> a
unJust (Just a) = a
