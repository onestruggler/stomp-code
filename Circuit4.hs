module Circuit4 where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor
import Data.List
import qualified Fast as F
import GateStruct
import QuipperParser
import TfcParser

desugar = F.desugar_cir

allCli :: [Gate] -> Bool
allCli = all F.isCliffordg

type LR a = StateT ([Gate], [Gate]) Maybe a

moveh_step :: [Gate] -> LR [Gate]
moveh_step [] = mzero
moveh_step [a]
  | F.isCliffordg a = do
    (l, r) <- get
    put (l, a : r)
    return []
moveh_step (H i : H j : t)
  | i == j = return t
moveh_step (H i : H j : t)
  | null t = do
    (l, r) <- get
    put (l, H i : H j : r)
    return []
  | otherwise =
    do
      t' <- moveh_step $ H i : t
      return $ H j : t'
      <|> do
        t' <- moveh_step $ H j : t
        moveh_step $ H i : t'
moveh_step (H i : T j : t)
  | i /= j =
    do
      t' <- moveh_step $ H i : t
      return $ T j : t'
      <|> do
        return $ [T j, H i] ++ t
moveh_step (H i : S j : H k : t)
  | i == j && j == k = return $ Z i : S i : H i : S i : Z i : t
moveh_step (H i : S j : t)
  | i == j = do
    t' <- moves_step $ S j : t
    moveh_step $ H i : t'
  | otherwise = do
    t' <- moveh_step $ H i : t
    return $ S j : t'
moveh_step (H i : Z j : t)
  | i == j = do
    t' <- moveh_step $ H i : t
    return $ X j : t'
  | otherwise = do
    t' <- moveh_step $ H i : t
    return $ Z j : t'
moveh_step (H i : CZ j k : t)
  | i == j = do
    t' <- moveh_step $ H i : t
    return $ CX j k : t'
  | i == k = do
    t' <- moveh_step $ H i : t
    return $ CX k j : t'
  | otherwise = do
    t' <- moveh_step $ H i : t
    return $ CZ j k : t'
moveh_step (H i : CCZ j k m : t)
  | i `notElem` [j, k, m] =
    do
      t' <- moveh_step $ H i : t
      return $ CCZ j k m : t'
      <|> do
        return $ CCZ j k m : H i : t
moveh_step (H i : X j : t)
  | i == j = do
    t' <- moveh_step $ H i : t
    return $ Z j : t'
  | i /= j = do
    t' <- moveh_step $ H i : t
    return $ X j : t'
moveh_step (H i : CX j k : t)
  | i == j =
    do
      t' <- moveh_step $ H i : t
      return $ CZ j k : t'
      <|> do
        return $ [CZ j k, H i] ++ t
  | i /= j && i /= k = do
    t' <- moveh_step $ H i : t
    return $ CX j k : t'
moveh_step (H i : CX j k : H l : t)
  | i == k && i == l =
    do
      t' <- moveh_step $ H j : t
      return $ [H j, CX k j] ++ t'
      <|> do
        t' <- moveh_step $ H l : t
        moveh_step $ H i : CX j k : t'
  | i == k && j == l =
    do
      t' <- moveh_step $ H i : t
      return $ [H j, CX i j] ++ t'
      <|> do
        t' <- moveh_step $ H j : t
        moveh_step $ H i : CX j k : t'
moveh_step (H i : CX j k : S l : H m : t)
  | i == k && i == l && i == m =
    do
      t' <- moveh_step $ H j : t
      return $ [H j, CX k j, Z k, S k, H k, Z k, S k] ++ t'
      <|> do
        t' <- moveh_step $ H m : t
        moveh_step $ [H i, CX j k, S l] ++ t'
  | i == k && j == l && j == m = mzero --do tricky
  --      moveh_step $ [H i, CZ i j, S j, CX j k, H j, S k] ++ t
moveh_step (H i : CX j k : t)
  | i /= j && i == k = do
    t' <- movecx_step $ CX j k : t
    moveh_step $ H i : t'
moveh_step (H i : Swap j k : t)
  | i == j = do
    t' <- moveh_step $ H k : t
    return $ Swap j k : t'
  | i == k = do
    t' <- moveh_step $ H j : t
    return $ Swap j k : t'
  | otherwise = do
    t' <- moveh_step $ H i : t
    return $ Swap j k : t'
moveh_step _ = mzero

movehn 0 cir = do
  (l, r) <- get
  put (reverse cir ++ l, r)
  return []
movehn n [] = mzero
movehn n [a] = do
  (l, r) <- get
  put (a : l, r)
  return []
movehn n xs@(H i : t) =
  do
    t' <- moveh_step xs
    movehn (n -1) t'
    <|> do
      (l, r) <- get
      put (H i : l, r)
      movehn (n -1) t
movehn n xs@(h : t) = do
  (l, r) <- get
  put (h : l, r)
  movehn (n -1) t

moveh :: [Gate] -> LR [Gate]
moveh [] = mzero
moveh [a] = do
  (l, r) <- get
  put (a : l, r)
  return []
moveh xs@(H i : t) =
  do
    t' <- moveh_step xs
    moveh t'
    <|> do
      (l, r) <- get
      put (H i : l, r)
      moveh t
moveh xs@(h : t) = do
  (l, r) <- get
  put (h : l, r)
  moveh t

moves_step :: [Gate] -> LR [Gate]
moves_step [] = mzero
moves_step [a]
  | F.isCliffordg a = do
    (l, r) <- get
    put (l, a : r)
    return []
moves_step xs@(S i : H j : t)
  | i == j = return xs
  | otherwise = return $ H j : S i : t
moves_step (S i : T j : t) = return $ T j : S i : t
moves_step (S i : S j : t)
  | i == j = return $ Z j : t
  | otherwise = return $ S j : S i : t
moves_step (S i : Z j : t) = return $ Z j : S i : t
moves_step (S i : CZ j k : t) = return $ CZ j k : S i : t
moves_step (S i : CCZ j k l : t) = return $ CCZ j k l : S i : t
moves_step (S i : X j : t)
  | i == j = return $ X j : Z i : S i : t
  | otherwise = return $ X j : S i : t
moves_step (S i : CX j k : t)
  | i == j = return $ CZ j k : CX j k : S i : Z k : S k : t
  | otherwise = return $ CX j k : S i : t
moves_step (S i : Swap j k : t)
  | i == j = return $ Swap j k : S k : t
  | i == k = return $ Swap j k : S j : t
  | otherwise = return $ Swap j k : S i : t

movecx_step :: [Gate] -> LR [Gate]
movecx_step [] = mzero
movecx_step [a]
  | F.isCliffordg a = do
    (l, r) <- get
    put (l, a : r)
    return []
movecx_step xs@(CX i i' : H j : t)
  | j `elem` [i, i'] = return xs
  | otherwise = return $ H j : CX i i' : t
movecx_step (CX i i' : T j : t)
  | j == i = mzero -- return $ CX i i' : T j : t
  | otherwise = return $ T j : CX i i' : t
movecx_step xs@(CX i i' : S j : H k : t)
  | j `elem` [i, i'] && j == k = return xs
movecx_step (CX i i' : S j : t)
  | j `elem` [i, i'] = do
    t' <- moves_step $ S j : t
    movecx_step $ CX i i' : t'
  | otherwise = return $ S j : CX i i' : t
movecx_step (CX i i' : Z j : t)
  | i == j = return $ [Z j, Z i', CX i i'] ++ t
  | otherwise = return $ [Z j, CX i i'] ++ t
movecx_step (CX i i' : CZ j j' : t)
  | (i == j && i' == j') || (i == j' && i' == j) =
    return $ [CZ j j', Z i', CX i i'] ++ t
  | i == j && i' /= j' =
    return $ [CZ j j', CZ i' j', CX j' i'] ++ t
  | i == j' && i' /= j =
    return $ [CZ j j', CZ i' j, CX j i'] ++ t
  | (i' == j && i /= j') || (i' == j' && i /= j) =
    return $ [CZ j j', CX i i'] ++ t
  | i /= j && i /= j' && i' /= j && i' /= j' =
    return $ [CZ j j', CX i i'] ++ t
movecx_step (CX i i' : CCZ j j' j'' : t)
  | i `elem` [j, j', j''] = mzero -- return $ CX i i' : CCZ j j' j'' : t
  | otherwise = return $ [CCZ j j' j'', CX i i'] ++ t
movecx_step (CX i i' : X j : t)
  | i' == j = return $ [X j, X i, CX i i'] ++ t
  | otherwise = return $ [X j, CX i i'] ++ t
movecx_step (CX i i' : CX j j' : t)
  | i == j && i' == j' = return t
  | i == j' && i' == j = return $ [Swap j j', CX i i'] ++ t
  | i == j && i' /= j' = return $ [CX j j', CX i i'] ++ t
  | i == j' && i' /= j = return $ [CX j j', CX i i', CX j i'] ++ t
  | i' == j && i /= j' = return $ [CX j j', CX i i', CX i j'] ++ t
  | i' == j' && i /= j = do
    t' <- moveh_step $ H i' : CX j j' : t
    return $ [CX i i', H i'] ++ t'
  | (i /= j && i' /= j') && (i /= j' && i' /= j) = do
    t' <- movecx_step $ CX i i' : t
    return $ CX j j' : t'
movecx_step (CX i i' : Swap j j' : t) =
  case (length . sort . nub) [i, i', j, j'] of
    2 -> return $ [Swap j j', CX i' i] ++ t
    3 -> if i == j then return $ [Swap j j', CX j' i'] ++ t else (if i == j' then return $ [Swap j j', CX j i'] ++ t else (if i' == j then return $ [Swap j j', CX i j'] ++ t else return $ [Swap j j', CX i j] ++ t))
    4 -> return $ [Swap j j', CX i i'] ++ t

movecx :: [Gate] -> LR [Gate]
movecx [] = mzero
movecx [a] = do
  (l, r) <- get
  put (a : l, r)
  return []
movecx xs@(CX i j : t) =
  do
    t' <- movecx_step xs
    movecx t'
    <|> do
      (l, r) <- get
      put (CX i j : l, r)
      movecx t
movecx xs@(h : t) = do
  (l, r) <- get
  put (h : l, r)
  movecx t

movecxccx_step (CX i i' : CCX j j' j'' : t)
  | i' == j = return $ [CCX j j' j'', CCX i j' j'', CX i i'] ++ t
  | otherwise = return $ [CCX j j' j'', CX i i'] ++ t
movecxccx_step [] = Nothing
movecxccx_step (h : t) = do
  t' <- movecxccx_step t
  return $ h : t'

movecxccx_step' (CX i i' : CCX j j' j'' : t)
  | i' == j = return $ [CCX j j' j'', CCX i j' j'', CX i i'] ++ t
  | otherwise = return $ [CCX j j' j'', CX i i'] ++ t

movecxccx :: [Gate] -> LR [Gate]
movecxccx [] = mzero
movecxccx [a@(CX i j)] = do
  (l, r) <- get
  put (l, a : r)
  return []
movecxccx xs@(CX i j : CX i' j' : t) = do
  t' <- movecxccx (CX i' j' : t)
  movecxccx $ CX i j : t'
movecxccx xs@(CX i i' : CCX j j' j'' : t) = do
  t' <- movecxccx_step' xs
  movecxccx t'
movecxccx xs@(h : t) = do
  t' <- movecxccx t
  return $ h : t'

type LR' a = State ([Gate], [Gate]) a

movegf_i :: [Gate] -> LR' ([Gate], Int)
movegf_i [a]
  | F.isCliffordg a = do
    (l, r) <- get
    put (l, a : r)
    return ([], 0)

{-
movegf_i (CX i j : Toffoli k l m)
  | i == l && j `member` ws = do
      (t', n') <- movegf_i $ CX i j : t
      return $ (Gad (G p wsi) : t', if p `mod` 2 == 1 then n' - 1 else n')
  | i `member` ws = do
      (t', n') <- movegf_i $ CX i j : t
      return $ (Gad (G p wsj) : t' ,  if p `mod` 2 == 1 then n' + 1 else n')
  | length.nub [i,j,k,l,m] == 5 || i == k
    || (length.nub [i,j,k,l,m] == 4 && (j == l || j == m)) = do
      (t', n') <- movegf_i $ CX i j : t
      return $ (Toffoli k l m : t', n')
  where
    wsj = Set.insert j ws
    wsi = Set.delete j ws
-}

{-
movegf_i (CX i j : CX k l : t)
  | (i == k && j == l) = return $ (t,0)
  | (i == l && j == k) = do
      (t',n') <- movegf_i $ CX i j : t
      let (t'', sw) = moveswap' (Swap i j : t')
      (l,r) <- get
      put (l, sw ++ r)
      return (t'', n')
  | (i == l && j /= k) = do
       (t', n') <- movegf_i $ CX i j : t
       (t'', n'') <- movegf_i $ CX k j : t'
       return $ (CX k l : t'', n'' + n')
  | (i /= l && j == k) = do
       (t', n') <- movegf_i $ CX i j : t
       (t'', n'') <- movegf_i $ CX i l : t'
       return $ (CX k l : t'', n'' + n')
  | otherwise = do
      (t', n') <- movegf_i $ CX i j : t
      return $ (CX k l : t', n')

movegf_i' :: [Gate] -> (([Gate], Int), ([Gate],[Gate]))
movegf_i' zx = runState (movegf_i zx) ([],[])

movegf :: ([Gate],[Gate]) -> LR' ([Gate],[Gate])
movegf (m, CX i j : t) = do
  let ((tr, nr), (lr, rr)) = movegf_i' $ CX i j : t
  let ((tl, nl), (ll, rl)) = movegf_i' $ CX i j : m
  case nr < nl of
    True -> do
      (l, r) <- get
      put (l, rr ++ r)
      movegf (m,tr)
    False -> do
      (l, r) <- get
      put (l ++ reverse rl, r)
      movegf (tl, t)
movegf (m, h : t) = do
  movegf (h:m, t)
movegf (m, []) = return (m, [])

movegf' :: [Gate] -> ([Gate], [Gate], [Gate])
movegf' zx = (l,m,r) where
  ((ml,mr),(l,r)) = runState (movegf ([],zx)) ([],[])
  m = reverse ml

-}

cnot2cx :: Gate -> Gate
cnot2cx (Cnot i j) = CX i j
cnot2cx g = g

cx2cnot :: Gate -> Gate
cx2cnot (CX i j) = Cnot i j
cx2cnot g = g

-- ----------------------------------------------------------------------

-- * Useful general-purpose functions

-- | Repeatedly apply a one-step reduction until it's done.
repeatedly :: (a -> Maybe a) -> (a -> a)
repeatedly f a = case f a of
  Nothing -> a
  Just b -> repeatedly f b

mvH f cir = (f cir >>= mvH f) <|> return cir

mvH' f cir = f cir >>= mvH' f

{-
mvh_n' n cir = moveh cir >>= mvh_n' (n-1)
mvh_n n cir = (reverse $ fst $ snd  mlr, snd $ snd mlr)
  where
    mlr = runState ( runMaybeT (mvh_n' n cir)) ([],[])
-}

mvh cir = Data.Bifunctor.first reverse lr
  where
    lr = unJust $ execStateT (moveh cir) ([], [])

mvcx cir = Data.Bifunctor.first reverse lr
  where
    lr = unJust $ execStateT (movecx cir) ([], [])

mvcxccx' cir = (m, snd lr)
  where
    lr = snd mlr
    m = fst mlr
    mlr = unJust $ runStateT (movecxccx cir) ([], [])

isCX (CX _ _) = True
isCX _ = False

mvcxccx cir = (l, r)
  where
    lr = reverse $ repeatedly movecxccx_step cir
    r = reverse $ takeWhile isCX lr
    l = reverse $ dropWhile isCX lr

mvhn n cir = Data.Bifunctor.first reverse lr
  where
    lr = unJust $ execStateT (movehn n cir) ([], [])

unJust :: Maybe a -> a
unJust (Just a) = a
