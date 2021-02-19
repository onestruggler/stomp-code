module Squeeze3 where

import Control.Monad.State
--import Data.HashMap.Strict (HashMap)
--import qualified Data.HashMap.Strict as Map
import Data.Hashable
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
--import ZX8

import Fast
import GateStruct
import QuipperParser
import TfcParser2

-- | Gate' which ignore the the qubit it acts on. This type is just for
-- efficiency reasons.
data Gate'
  = H'
  | X'
  | CX'
  | CCX'
  | T'
  | S'
  | Z'
  | CZ'
  | CCZ'
  | Swap'
  deriving (Eq, Ord, Read, Show)

gate'_of_gate :: Gate -> Gate'
gate'_of_gate (H _) = H'
gate'_of_gate (X _) = X'
gate'_of_gate (CX _ _) = CX'
gate'_of_gate (CCX _ _ _) = CCX'
gate'_of_gate (T _) = T'
gate'_of_gate (S _) = S'
gate'_of_gate (Z _) = Z'
gate'_of_gate (CZ _ _) = CZ'
gate'_of_gate (CCZ _ _ _) = CCZ'
gate'_of_gate (Swap _ _) = Swap'

wires_of_gate :: Gate -> [Int]
wires_of_gate (H i) = [i]
wires_of_gate (X i) = [i]
wires_of_gate (CX i j) = [i, j]
wires_of_gate (CCX i j k) = [i, j, k]
wires_of_gate (T i) = [i]
wires_of_gate (S i) = [i]
wires_of_gate (Z i) = [i]
wires_of_gate (CZ i j) = [i, j]
wires_of_gate (CCZ i j k) = [i, j, k]
wires_of_gate (Swap i j) = [i, j]
wires_of_gate (Ga p ws) = Set.toList ws

controls_of_gate :: Gate -> [Int]
controls_of_gate (H i) = []
controls_of_gate (X i) = []
controls_of_gate (CX i j) = [j]
controls_of_gate (CCX i j k) = [j, k]
controls_of_gate (T i) = []
controls_of_gate (S i) = []
controls_of_gate (Z i) = []
controls_of_gate (CZ i j) = [i, j]
controls_of_gate (CCZ i j k) = [i, j, k]
controls_of_gate (Swap i j) = []
controls_of_gate (Ga p ws) = Set.toList ws

targets_of_gate :: Gate -> [Int]
targets_of_gate (H i) = []
targets_of_gate (X i) = [i]
targets_of_gate (CX i j) = [i]
targets_of_gate (CCX i j k) = [i]
targets_of_gate (T i) = []
targets_of_gate (S i) = []
targets_of_gate (Z i) = []
targets_of_gate (CZ i j) = []
targets_of_gate (CCZ i j k) = []
targets_of_gate (Swap i j) = []
targets_of_gate (Ga p ws) = []

similar_gate :: Gate -> Gate -> Bool
similar_gate (H _) (H _) = True
similar_gate (T _) (T _) = True
similar_gate (S _) (S _) = True
similar_gate (Z _) (Z _) = True
similar_gate (CZ _ _) (CX _ _) = True
similar_gate (CCZ _ _ _) (CCZ _ _ _) = True
similar_gate (X _) (X _) = True
similar_gate (CX _ _) (CX _ _) = True
similar_gate (CCX _ _ _) (CCX _ _ _) = True
similar_gate (Swap _ _) (Swap _ _) = True
similar_gate (Ga p ws) (Ga p' ws')
  | (p + p') `mod` 2 /= 1 && Set.size ws == Set.size ws' = True
similar_gate (_) (_) = False

squeezec :: [Gate] -> [Column]
squeezec xs = foldl' (flip assign_col) [] (reverse xs)

squeezec' :: [Gate] -> [Column] -> [Column]
squeezec' xs xss = foldl' (flip assign_col) xss (reverse xs)

squeeze :: [Gate] -> [Column]
squeeze xs = foldl' (flip assign_col) [] (reverse xs)

squeeze' :: [Gate] -> [Column] -> [Column]
squeeze' xs xss = foldl' (flip assign_col) xss (reverse xs)

squeeze_SM :: [Gate] -> [Column]
squeeze_SM xs = foldl' (flip assign_col') [] (reverse xs)

squeeze_SM' :: [Gate] -> [Column] -> [Column]
squeeze_SM' xs xss = foldl' (flip assign_col') xss (reverse xs)

commute_gate :: Gate -> Gate -> Bool
commute_gate a b
  | a == b = False
commute_gate (H i) (CCZ j k l) = i /= j && i /= k && i /= l
commute_gate (CCZ j k l) (H i) = i /= j && i /= k && i /= l
commute_gate (H _) (H _) = True
commute_gate (CCZ i j k) (CCZ i' j' k') = sort [i, j, k] /= sort [i', j', k']
commute_gate (CX i j) (CCZ i' j' k') = i /= i' && i /= j' && i /= k'
commute_gate (CCZ i' j' k') (CX i j) = i /= i' && i /= j' && i /= k'
commute_gate (CX i j) (H k) = k /= i && k /= j
commute_gate (H k) (CX i j) = k /= i && k /= j
commute_gate (CX i j) (CX i' j') = i /= j' && j /= i'
commute_gate (CZ _ _) (CCZ _ _ _) = True
commute_gate (CCZ _ _ _) (CZ _ _) = True
commute_gate (CZ i j) (CX i' j') = i' /= i && i' /= j
commute_gate (CX i' j') (CZ i j) = i' /= i && i' /= j
commute_gate (CZ i j) (CZ i' j') = sort [i, j] /= sort [i', j']
commute_gate (CCX i j k) (CCX i' j' k') =
  i /= j' && i /= k' && i' /= j && i' /= k
commute_gate (CX i j) (CCX i' j' k') =
  i /= j' && i /= k' && i' /= j
commute_gate (CCX i' j' k') (CX i j) =
  i /= j' && i /= k' && i' /= j
commute_gate (Ga p ws) (Ga p' ws') = True
commute_gate a b
  | aib == [] = True
  | all (`elem` ac) aib && all (`elem` bc) aib = True
  | at == bt && at == aib = True
  | at == bt && (all (`elem` ac) aib' && all (`elem` bc) aib') = True
  | otherwise = False
  where
    aw = wires_of_gate a
    bw = wires_of_gate b
    aib = aw `intersect` bw
    ac = controls_of_gate a
    bc = controls_of_gate b
    at = targets_of_gate a
    bt = targets_of_gate b
    aib' = aib \\ at

overlap_gate :: Gate -> Gate -> Bool
overlap_gate (H i) (CCZ j k l) = i == j || i == k || i == l
overlap_gate (CCZ j k l) (H i) = i == j || i == k || i == l
overlap_gate (H i) (H i') = i == i'
overlap_gate (CCZ i j k) (CCZ i' j' k') = [i, j, k] `intersect` [i', j', k'] /= []
overlap_gate (CX i j) (CCZ i' j' k') = [i, j] `intersect` [i', j', k'] /= []
overlap_gate (CCZ i' j' k') (CX i j) = [i, j] `intersect` [i', j', k'] /= []
overlap_gate (CX i j) (H k) = k == i || k == j
overlap_gate (H k) (CX i j) = k == i || k == j
overlap_gate (CX i j) (CX i' j') = [i, j] `intersect` [i', j'] /= []
overlap_gate (CZ i j) (CCZ i' j' k') = [i, j] `intersect` [i', j', k'] /= []
overlap_gate (CCZ i' j' k') (CZ i j) = [i, j] `intersect` [i', j', k'] /= []
overlap_gate (CZ i j) (CX i' j') = [i, j] `intersect` [i', j'] /= []
overlap_gate (CX i j) (CZ i' j') = [i, j] `intersect` [i', j'] /= []
overlap_gate (CZ i j) (CZ i' j') = [i, j] `intersect` [i', j'] /= []
overlap_gate (CCX i j k) (CCX i' j' k') = [i, j, k] `intersect` [i', j', k'] /= []
overlap_gate (CX i j) (CCX i' j' k') = [i, j] `intersect` [i', j', k'] /= []
overlap_gate (CCX i' j' k') (CX i j) = [i, j] `intersect` [i', j', k'] /= []
overlap_gate a b
  | aib == [] = False
  | otherwise = True
  where
    aw = wires_of_gate a
    bw = wires_of_gate b
    aib = aw `intersect` bw

instance Hashable Gate' where
  hashWithSalt s H' = 1
  hashWithSalt s X' = 2
  hashWithSalt s CX' = 3
  hashWithSalt s CCX' = 4
  hashWithSalt s T' = 5
  hashWithSalt s S' = 6
  hashWithSalt s Z' = 7
  hashWithSalt s CZ' = 8
  hashWithSalt s CCZ' = 9
  hashWithSalt s Swap' = 0

type Column = (Map Int Gate, Map Gate' [Gate])

type SqueezedC = [Column]

empty_col :: Column
empty_col = (Map.empty, Map.empty)

singleton_col x = insert_col x empty_col

col_of_gates :: [Gate] -> Column
col_of_gates gs = insert_col_m gs empty_col

squee_of_gatess :: [[Gate]] -> [Column]
squee_of_gatess xss = map col_of_gates xss

gates_of_col :: Column -> [Gate]
gates_of_col (mig, mgg) = concat (Map.elems mgg)

gates_of_cols cols = concat $ map gates_of_col cols

gatess_of_cols cols = map gates_of_col cols

insert_col_m xs col = foldl' (flip insert_col) col xs

insert_col :: Gate -> Column -> Column
insert_col g (mig, mgg) = (mig', mgg')
  where
    mig' = foldl' (flip (\k -> Map.insert k g)) mig gw
    mgg' = Map.insertWith (++) (gate'_of_gate g) [g] mgg
    gw = wires_of_gate g

overlap_gc :: Gate -> Column -> Bool
overlap_gc g (mig, mgg) = not $ all (\x -> Map.lookup x mig == Nothing) (wires_of_gate g)

commute_gc :: Gate -> Column -> Bool
commute_gc g (mig, mgg) = b
  where
    potential_blocker = map (\x -> Map.lookup x mig) (wires_of_gate g)
    commute_m :: Gate -> Maybe Gate -> Bool
    commute_m a (Just b) = commute_gate a b
    commute_m a Nothing = True
    b = all (commute_m g) potential_blocker

assign_col :: Gate -> [Column] -> [Column]
assign_col a [] = [insert_col a empty_col]
assign_col g (h : [])
  | not (overlap_gc g h) = (insert_col g h) : []
  | otherwise = (singleton_col g) : h : []
assign_col a (h : t)
  | commute_gc a h = case gates_of_col (head t') == [a] of
    True -> case overlap_gc a h of
      True -> singleton_col a : h : t
      False -> insert_col a h : t
    _ -> h : t'
  | otherwise = singleton_col a : h : t
  where
    t' = assign_col a t

-- | not efficient compared to also computing "commuting", kind of wierd.
assign_col' :: Gate -> [Column] -> [Column]
assign_col' a [] = [insert_col a empty_col]
assign_col' g (h : [])
  | not (overlap_gc g h) = (insert_col g h) : []
  | otherwise = (singleton_col g) : h : []
assign_col' a (h1 : h2 : t)
  | not (overlap_gc a h1) = case not (overlap_gc a h2) of
    True -> h1 : t'
    False -> insert_col a h1 : h2 : t
  | otherwise = singleton_col a : h1 : h2 : t
  where
    t' = assign_col a (h2 : t)

-- | Each gate takes one or more 'Int' values as arguments. We regard
-- an argument as a parameter if it greater than or equal to zero, as
-- a varialbe if it is less than zero. e.g. "H 1" is a gate acting on
-- the 1st qubit. "H -1" is an H gate acting on some qubit to be found
-- later, i.e., we think of "-1" as a variable to be instantiated
-- later.

-- | a variable is an int less than zero.
type Variable = Int

-- | a binding is a pair of variable and int.
type Subst1 = (Variable, Int)

type Subst = [Subst1]

-- | This way, we can use the same structure to represent a circuit
-- and an abstruct rewrite rule, since a rewrite rule say "[H -1, H
-- -1] -> []" always stands for many such rule with different "-1"s.

-- | An abstract rule is represented by a pair of squeezed circuits
-- with wire variables. If we want to find matches of the LHS, we are
-- acutally unifying the LHS with some concrete circuit.

-- | "match a b" matches a squeezed circuit "a" containing variables
-- with the first several columns of another squeezed circuit "b".
match :: [[Gate]] -> [Column] -> Maybe Subst
match [] _ = Just []
match (h : t) [] = Nothing
match (h : t) sc@(h'@(mig, mgg) : t') = case match_col h h' of
  Nothing -> Nothing
  Just bh -> case match (instantiate' bh t) t' of
    Nothing -> match (h : t) sc'
    Just bt -> return $ bh ++ bt
    where
      h'' = delete_col (head (instantiate_sgs bh h)) h'
      sc' = h'' : t'

delete_col :: Gate -> Column -> Column
delete_col g (mig, mgg) = (mig', mgg')
  where
    mig' = foldl' (flip Map.delete) mig (wires_of_gate g)
    mgg' = Map.update (f g) (gate'_of_gate g) mgg
    f g x = if x == [] || x == [g] then Nothing else Just (delete g x)

delete_col_m xs col = foldl' (flip delete_col) col xs

instantiate1 :: Subst1 -> Gate -> Gate
instantiate1 (l, r) _
  | l >= 0 = error "instantiate1: incorrect binding, LHS is not a variable"
instantiate1 (l, r) (H i)
  | l == i = H r
instantiate1 (l, r) (T i)
  | l == i = T r
instantiate1 (l, r) (S i)
  | l == i = S r
instantiate1 (l, r) (Z i)
  | l == i = Z r
instantiate1 (l, r) (CZ i j)
  | l == i = CZ r j
  | l == j = CZ i r
instantiate1 (l, r) (CCZ i j k)
  | l == i = CCZ r j k
  | l == j = CCZ i r k
  | l == k = CCZ i j r
instantiate1 (l, r) (X i)
  | l == i = X r
instantiate1 (l, r) (CX i j)
  | l == i = CX r j
  | l == j = CX i r
instantiate1 (l, r) (CCX i j k)
  | l == i = CCX r j k
  | l == j = CCX i r k
  | l == k = CCX i j r
instantiate1 (l, r) (Swap i j)
  | l == i = Swap r j
  | l == j = Swap i r
instantiate1 _ g = g

instantiate_sg :: Subst -> Gate -> Gate
instantiate_sg [] g = g
instantiate_sg (h : t) g = instantiate_sg t (instantiate1 h g)

instantiate_sgs :: Subst -> [Gate] -> [Gate]
instantiate_sgs [] c = c
instantiate_sgs (h : t) c = instantiate_sgs t $ map (instantiate1 h) c

instantiate_col :: Subst -> Column -> Column
instantiate_col (bs) (mig, mgg) = (mig', mgg')
  where
    mig' = Map.map (instantiate_sg bs) mig
    mgg' = Map.map (map (instantiate_sg bs)) mgg

instantiate' :: Subst -> [[Gate]] -> [[Gate]]
instantiate' [] xs = xs
instantiate' (h : t) xs = instantiate' t xs'
  where
    xs' = map (\ys -> map (instantiate1 h) ys) xs

match_g_g's :: Gate -> [Gate] -> Maybe Subst
match_g_g's g [] = Nothing
match_g_g's g (h : t) = case unify_gate g h of
  Nothing -> match_g_g's g t
  Just b -> Just b

match_col :: [Gate] -> Column -> Maybe Subst
match_col [] _ = Just []
match_col (h : t) (mig, mgg) = case Map.lookup (gate'_of_gate h) mgg of
  Nothing -> Nothing
  Just [] -> Nothing
  Just h's -> match_g_g's h h's

unify_gate :: Gate -> Gate -> Maybe Subst
unify_gate (H i) (H j)
  | i == j = Just []
  | i < 0 = Just [(i, j)]
  | j < 0 = Just [(j, i)]
  | otherwise = Nothing
unify_gate (T i) (T i') = unify_gate (H i) (H i')
unify_gate (S i) (S i') = unify_gate (H i) (H i')
unify_gate (Z i) (Z i') = unify_gate (H i) (H i')
unify_gate (CZ i j) (CZ i' j')
  | i == i' && j == j' = Just []
  | i /= i' && j == j' = case i < 0 of
    True -> Just [(i, i')]
    False -> case i' < 0 of
      True -> Just [(i', i)]
      False -> Nothing
  | i == i' && j /= j' = case j < 0 of
    True -> Just [(j, j')]
    False -> case j' < 0 of
      True -> Just [(j', j)]
      False -> Nothing
  | i < 0 && j < 0 = Just [(i, i'), (j, j')]
  | i' < 0 && j' < 0 = Just [(i', i), (j', j)]
  | i < 0 && j' < 0 = Just [(i, i'), (j', j)]
  | i' < 0 && j < 0 = Just [(j, j'), (i', i)]
unify_gate (CCZ i j k) (CCZ i' j' k')
  | i == i' && j == j' && k == k' = Just []
  | i /= i' && j == j' && k == k' = unify_gate (Z i) (Z i')
  | i == i' && j /= j' && k == k' = unify_gate (Z j) (Z j')
  | i == i' && j == j' && k /= k' = unify_gate (Z k) (Z k')
  | i /= i' && j /= j' && k == k' = unify_gate (CZ i j) (CZ i' j')
  | i == i' && j /= j' && k /= k' = unify_gate (CZ j k) (CZ j' k')
  | i /= i' && j == j' && k == k' = unify_gate (CZ i k) (CZ i' k')
  | i < 0 && j < 0 && k < 0 = Just [(i, i'), (j, j'), (k, k')]
  | i' < 0 && j' < 0 && k' < 0 = Just [(i', i), (j', j), (k, k')]
  | i' < 0 && j < 0 && k < 0 = Just [(j, j'), (i', i), (k, k')]
  | i < 0 && j' < 0 && k < 0 = Just [(i, i'), (j', j), (k, k')]
  | i < 0 && j < 0 && k' < 0 = Just [(i, i'), (j', j), (k', k)]
  | i' < 0 && j' < 0 && k < 0 = Just [(j', j), (i', i), (k, k')]
  | i < 0 && j' < 0 && k' < 0 = Just [(i, i'), (j, j'), (k', k)]
  | i' < 0 && j < 0 && k' < 0 = Just [(i', i), (j, j'), (k', k)]
unify_gate (X i) (X j) = unify_gate (H i) (H j)
unify_gate (CX i j) (CX i' j') = unify_gate (CZ i j) (CZ i' j')
unify_gate (CCX i j k) (CCX i' j' k') = unify_gate (CCZ i j k) (CCZ i' j' k')
unify_gate (Swap i j) (Swap i' j') = unify_gate (CZ i j) (CZ i' j')
unify_gate _ _ = Nothing

-- | we need record the correspondence between the columns of the RHS
-- and of the LHS, by this info, we can make sure after rewriting the
-- RHS columns are placed in the right position in a squeezed circuit.

-- | note that the RHS is also a pair, the fst part of it is the
-- columns before "0th column" (the column in RHS correspoding to the
-- 0th element of the LHS), the snd part of it is the clolumn after
-- 0th column (including 0).

-- | we also make the LHS a pair, the snd of it works as a "guard"
-- i.e. if further match of "snd l" can be made, then don't do this
-- rule. "snd l" is empty means no "guard", i.e. ignore the guard,
-- perform the rewrite.

-- | these definition makes the rules hard to write, but that make the
-- rewriting procedure easier to implement.

-- | be careful when translating the usual rules to the rules in this
-- format.
-- | note that 1) adding some I gate to ensure the LHS is convex. 2)
-- add a pair of numbers to indicate how to separate columns the LHS
-- being matched on, i.e indicate which columns are put before the RHS
-- and which are put after the RHS after rewriting. 3) The LHS is a
-- pair, the second part of it if being matched indicate we don't do
-- the rewriting.
type LHS = [[Gate]]

type LHS_Guard = [[Gate]]

type RHS = [[Gate]]

type Sep = Int

data Rule' = Rule LHS LHS_Guard RHS Sep

type Rules' = [Rule']

type Rule = (([[Gate]], [[Gate]]), ([[Gate]], [[Gate]]))

type Rules = [Rule]

instantiate_rule :: Subst -> Rule -> Rule
instantiate_rule b ((ll, lr), (rl, rr)) = ((ll', lr'), (rl', rr'))
  where
    [ll', lr', rl', rr'] = map (instantiate' b) [ll, lr, rl, rr]

instantiate_rule' :: Subst -> Rule' -> Rule'
instantiate_rule' b (Rule l lg r s) = Rule l' lg' r' s
  where
    [l', lg', r'] = map (instantiate' b) [l, lg, r]

-- | e.g. this rule ([[CX 1 2],[H 1]], [([H 1], -1)], ([CZ 1 2], 0)])
-- says "CZ 1 2" must be place in the first column, and "H 1" must
-- before "CZ 1 2".

-- | runRules repeatedly
runRules_rep :: Rules -> SqueezedC -> SqueezedC
runRules_rep rules sc
  | sc == sc' = sc
  | otherwise = runRules_rep rules sc'
  where
    sc' = runRules rules sc

-- | runRules repeatedly
runRules_rep' :: Rules' -> SqueezedC -> SqueezedC
runRules_rep' rules sc
  | sc == sc'' = sc
  | otherwise = runRules_rep' rules sc''
  where
    sc' = runRules' rules sc
    sc'' = squeeze $ concat $ map gates_of_col sc'

-- | runRules repeatedly
runRules_rep's :: Rules' -> SqueezedC -> SqueezedC
runRules_rep's rules sc
  | map fst sc == map fst sc' = sc
  | otherwise = runRules_rep's rules sc'
  where
    sc' = runRules's rules sc

runRules :: Rules -> SqueezedC -> SqueezedC
runRules rules xss = foldl' (flip runRule) xss rules

runRules' :: Rules' -> SqueezedC -> SqueezedC
runRules' rules xss = foldl' (flip runRule') xss rules

runRules's :: Rules' -> SqueezedC -> SqueezedC
runRules's rules xss = foldl' (flip runRule's) xss rules

-- | runRule trys to find many matches in a squeezed circuit, and
-- rewrite all of them.
runRule :: Rule -> [Column] -> [Column]
runRule rule [] = []
runRule rule@(l@(ll, lr), r@(rl, rr)) xss@(h : t) = case match ll xss of
  Nothing -> squeeze' (gates_of_col h) $ runRule rule t
  Just b -> case null lr' || match lr' (drop len xss) == Nothing of
    True -> runRule rule $ rewrite rule' xss
    False -> squeeze' [ha] $ runRule rule xss'
    where
      ha = head $ head $ fst $ fst rule'
      h' = delete_col ha h
      xss' = squeeze' (gates_of_col h') t
      len = length ll
      rule' = instantiate_rule b rule
      lr' = snd $ fst rule'

runRule' :: Rule' -> [Column] -> [Column]
runRule' rule [] = []
runRule' rule@(Rule l lg r sep) xss@(h : t) = case match l xss of
  Nothing -> h : runRule' rule t
  Just b -> case null lg' || match lg' (drop len xss) == Nothing of
    True -> runRule' rule $ rewrite' rule' xss
    False -> singleton_col ha : runRule' rule xss'
    where
      ha = head $ head l'
      h' = delete_col ha h
      xss' = h' : t
      len = length l
      rule'@(Rule l' lg' r' sep') = instantiate_rule' b rule

runRule's :: Rule' -> [Column] -> [Column]
runRule's rule [] = []
runRule's rule@(Rule l lg r sep) xss@(h : t) = case match l xss of
  Nothing -> squeeze_SM' (gates_of_col h) $ runRule's rule t
  Just b -> case null lg' || match lg' (drop len xss) == Nothing of
    True -> runRule's rule $ rewrite's rule' xss
    False -> squeeze_SM' [ha] $ runRule's rule xss'
    where
      ha = head $ head l'
      h' = delete_col ha h
      xss' = h' : t
      len = length l
      rule'@(Rule l' lg' r' sep') = instantiate_rule' b rule

rewrite :: Rule -> [Column] -> [Column]
rewrite (l@(ll, lr), r@(rl, rr)) xss = xss'
  where
    xssl = zipWith (\x y -> delete_col_m x y) ll xss
    xssr = zipWith (\x y -> insert_col_m x y) rr' (xssl ++ repeat (empty_col))
    xss' = squeeze' ((concat (rl' ++ map gates_of_col xssr))) (drop (length xssl) xss)
    lenl = length xssl
    lenrr = length rr
    rr' = take (max lenl lenrr) $ rr ++ repeat []
    rl' = rl

rewrite' :: Rule' -> [Column] -> [Column]
rewrite' rule@(Rule l lg r b4r) xss = xss'
  where
    rm_lhs = zipWith (\x y -> delete_col_m x y) l xss
    len_lhs = length rm_lhs
    rhs = squee_of_gatess r
    col_b4_rhs = take b4r rm_lhs
    col_af_rhs = drop b4r rm_lhs
    xss' = col_b4_rhs ++ rhs ++ col_af_rhs ++ (drop len_lhs) xss

rewrite's :: Rule' -> [Column] -> [Column]
rewrite's rule@(Rule l lg r b4r) xss = xss'
  where
    rm_lhs = zipWith (\x y -> delete_col_m x y) l xss
    len_lhs = length rm_lhs
    rhs = squee_of_gatess r
    col_b4_rhs = map gates_of_col $ take b4r rm_lhs
    col_af_rhs = drop b4r rm_lhs
    xss' = squeeze_SM' (concat (col_b4_rhs ++ r)) $ col_af_rhs ++ (drop len_lhs) xss

hrules =
  [ (([[H (-1)], [H (-1)]], []), ([], [])),
    --  ([[CX (-1) (-2)],[H (-1)]], ([[H (-1)]], [[CZ (-1) (-2)]])),
    (([[H (-1)], [CZ (-1) (-2)]], []), ([], [[], [CX (-1) (-2)], [H (-1)]])),
    -- for technical reasons, second such rule considering symmetry of CZ.
    (([[H (-2)], [CZ (-1) (-2)]], []), ([], [[], [CX (-2) (-1)], [H (-2)]])),
    -- similarly, when involving CCZ, we need 3 or more repeated rules
    -- considering the symmetry of CCZ.
    (([[H (-2)], [CX (-1) (-2)], [H (-1)], [CCZ (-3) (-4) (-1)]], []), ([], [[H (-1)], [CX (-2) (-1)], [H (-2)], [CCZ (-3) (-4) (-1)]])),
    (([[H (-2)], [CX (-1) (-2)], [H (-1)], [CCZ (-3) (-1) (-4)]], []), ([], [[H (-1)], [CX (-2) (-1)], [H (-2)], [CCZ (-3) (-1) (-4)]])),
    (([[H (-2)], [CX (-1) (-2)], [H (-1)], [CCZ (-1) (-4) (-3)]], []), ([], [[H (-1)], [CX (-2) (-1)], [H (-2)], [CCZ (-1) (-4) (-3)]])),
    (([[H (-1)], [CX (-1) (-2)]], []), ([], [[], [CZ (-1) (-2)], [H (-1)]]))
    --  ([[H (-1)], [S (-1)], [H (-1)]], [[Z (-1)], [S (-1)], [X (-1)], [H (-1)], [S (-1)]])
  ]

hrules' =
  [ Rule
      [[H (-1)], [H (-1)]]
      []
      []
      0,
    Rule
      [[H (-1)], [CZ (-1) (-2)]]
      []
      [[CX (-1) (-2)], [H (-1)]]
      1,
    Rule
      [[H (-2)], [CZ (-1) (-2)]]
      []
      [[CX (-2) (-1)], [H (-2)]]
      1,
    Rule
      [[H (-2)], [CX (-1) (-2)], [I (-2), H (-1)], [CCZ (-3) (-4) (-1)]]
      []
      [[H (-1)], [CX (-2) (-1)], [H (-2)], [CCZ (-3) (-4) (-1)]]
      3,
    Rule
      [[H (-2)], [CX (-1) (-2)], [I (-2), H (-1)], [CCZ (-3) (-1) (-4)]]
      []
      [[H (-1)], [CX (-2) (-1)], [H (-2)], [CCZ (-3) (-1) (-4)]]
      3,
    Rule
      [[H (-2)], [CX (-1) (-2)], [I (-2), H (-1)], [CCZ (-1) (-4) (-3)]]
      []
      [[H (-1)], [CX (-2) (-1)], [H (-2)], [CCZ (-1) (-4) (-3)]]
      3,
    Rule
      [[H (-1)], [CX (-1) (-2)]]
      []
      [[CZ (-1) (-2)], [H (-1)]]
      1,
    Rule
      [[H (-1)], [S (-1)], [H (-1)]]
      []
      [[Z (-1)], [S (-1)], [X (-1)], [H (-1)], [S (-1)]]
      0
  ]

cxccx_rules =
  [ -- similarly, when involving CCX, we need 2 or more repeated rules
    -- considering the symmetry of CCX.
    (([[CX (-1) (-2)], [CCX (-2) (-3) (-4)]], []), ([], [[], [CCX (-2) (-3) (-4)], [CCX (-1) (-3) (-4)], [CX (-1) (-2)]]))
  ]

cxccx_rules' =
  [ -- similarly, when involving CCX, we need 2 or more repeated rules
    -- considering the symmetry of CCX.
    Rule
      [[CX (-1) (-2)], [CCX (-2) (-3) (-4)]]
      []
      [[CCX (-2) (-3) (-4)], [CCX (-1) (-3) (-4)], [CX (-1) (-2)]]
      1,
    Rule
      [[CX (-1) (-2)], [CCX (-1) (-3) (-4)]]
      []
      [[CCX (-1) (-3) (-4)], [CX (-1) (-2)]]
      1
      --  Rule [[CX (-1) (-2)], [CCX (-5) (-3) (-4)]]  []
      --     [[CCX (-5) (-3) (-4)], [CX (-1) (-2)]] 1
  ]

adjust_CZ_CCZ (CCZ i j k) = CCZ i' j' k'
  where
    xs = sort [i, j, k]
    i' = xs !! 0
    j' = xs !! 1
    k' = xs !! 2
adjust_CZ_CCZ (CZ i j) = CZ (min i j) (max i j)
adjust_CZ_CCZ x = x

adjust_w = map adjust_CZ_CCZ

-- | Toffoli to CCX
tof2ccx :: Gate -> Gate
tof2ccx (Toffoli t c1 c2) = CCX t c1 c2
tof2ccx g = g

isH :: Gate -> Bool
isH (H _) = True
isH _ = False

isCX :: Gate -> Bool
isCX (CX _ _) = True
isCX _ = False

isCCX :: Gate -> Bool
isCCX (CCX _ _ _) = True
isCCX _ = False

halve_cxccx :: [Gate] -> ([Gate], [Gate])
halve_cxccx xs = (l, r)
  where
    cxs = filter isCX xs
    lcx = take (length cxs `div` 2 + 1) cxs
    rcx = drop (length cxs `div` 2 + 1) cxs
    lccx = takeWhile isCCX xs
    rccx = drop (length lccx + length cxs) xs
    l = lccx ++ lcx
    r = rcx ++ rccx

mvcxccx :: [Gate] -> ([Gate], [Gate])
mvcxccx xs = (m, r)
  where
    cols = runRules's cxccx_rules' $ squeeze_SM xs
    cols' = reverse $ gatess_of_cols cols
    cols'_cx = map (\xs -> partition isCX xs) cols'
    cols'_cx' = takeWhile (\(a, b) -> (not . null) a) cols'_cx
    cxs = reverse $ map fst cols'_cx'
    cols_wo_cx = map snd cols'_cx'
    cols'' = reverse $ cols_wo_cx ++ drop (length cols'_cx') cols'
    (m, r) = (concat cols'', concat cxs)

mv_cxccx :: [Gate] -> ([Gate], [Gate])
mv_cxccx xs = (m, r)
  where
    (m, r) = mv_cxccx_rep (xs, [])

mv_cxccx_rep :: ([Gate], [Gate]) -> ([Gate], [Gate])
mv_cxccx_rep (m, r) = (m', r')
  where
    xss = squeeze m
    xss' = runRules_rep' cxccx_rules' xss
    rc = gates_of_col $ last xss'
    (rcx, rm) = partition isCX rc
    (m', r') = case rcx == [] of
      True -> (m, r)
      False -> mv_cxccx_rep (m'', r'')
    m'' = concat (map gates_of_col $ take (length xss' -1) xss') ++ rm
    r'' = rcx ++ r

{-
mv_cxccx :: [Gate] -> ([Gate], [Gate])
mv_cxccx xs = (m, r) where
  xss = squeeze xs
  xss'= runRules_rep cxccx_rules xss
  rc = gates_of_col $ last xss'
  (r, rc') = partition isCX rc
  m = gates_of_cols (take (length xss' -1) xss') ++ rc'
-}

mvh :: [Gate] -> ([Gate], [Gate])
mvh xs = (m, r)
  where
    xss = squeeze xs
    xss' = runRules_rep hrules xss
    rc = gates_of_col $ last xss'
    (r, rc') = partition isH rc
    m = gates_of_cols (take (length xss' -1) xss') ++ rc'

show_rule :: Rule -> [[Gate]]
show_rule ((ll, lr), (rl, rr)) = ll' ++ [[M 0]] ++ rr'
  where
    ll' = map (reindexCir (+ n)) ll
    rr' = map (reindexCir (+ n)) rr
    n = - (minimum $ wiresOfCir $ concat $ ll ++ rr)

print_rule = topdf . show_rule
