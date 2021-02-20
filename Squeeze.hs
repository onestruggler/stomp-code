module Squeeze where

import Control.Monad.State
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
--import ZX8

import Fast
import GateStruct
import QuipperParser
import TfcParser2

{-
-- | Gate structrue
data Gate = H Int
            | X Int
            | CX Int Int
            | CCX Int Int Int
            | T Int
            | S Int
            | Z Int
            | CZ Int Int
            | CCZ Int Int Int
            | Swap Int Int
            deriving (Eq,Ord,Read,Show)
-}

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
similar_gate CCZ {} CCZ {} = True
similar_gate (X _) (X _) = True
similar_gate (CX _ _) (CX _ _) = True
similar_gate CCX {} CCX {} = True
similar_gate (Swap _ _) (Swap _ _) = True
similar_gate (Ga p ws) (Ga p' ws')
  | (p + p') `mod` 2 /= 1 && Set.size ws == Set.size ws' = True
similar_gate _ _ = False

squeeze :: [Gate] -> [[Gate]]
squeeze xs = foldl' (flip assign_column') [] (reverse xs)

squeeze' :: [Gate] -> [[Gate]] -> [[Gate]]
squeeze' xs xss = foldl' (flip assign_column') xss (reverse xs)

type LD a = State [[Gate]] a

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
commute_gate (CZ _ _) CCZ {} = True
commute_gate CCZ {} (CZ _ _) = True
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
  | null aib = True
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
  | null aib = False
  | otherwise = True
  where
    aw = wires_of_gate a
    bw = wires_of_gate b
    aib = aw `intersect` bw

commute_c_g :: [Gate] -> Gate -> Bool
commute_c_g col g = all (commute_gate g) col

overlap_c_g :: [Gate] -> Gate -> Bool
overlap_c_g col g = any (overlap_gate g) col

assign_column :: Gate -> [[Gate]] -> [[Gate]]
assign_column g [] = [[g]]
assign_column g [h]
  | all (commute_gate g) h && not (any (overlap_gate g) h) = [g : h]
  | all (commute_gate g) h = [ha, g : hb]
  | otherwise = [[g], h]
  where
    ol = filter (\x -> wiresOfGate x `intersect` wiresOfGate g /= []) h
    ha = ol
    hb = h \\ ha
{-assign_column g (h1:h2:[])
  | all (commute_gate g) h1 && all (commute_gate g) h2 && not (any (overlap_gate g) h2)
  = h1:(g:h2):[]
  | all (commute_gate g) h1 && all (commute_gate g) h2 = h1:h2:[g]:[]
  | all (commute_gate g) h1 && not (any (overlap_gate g) h1) = (g:h1) : h2 : []
  | all (commute_gate g) h1 = h1 : [g] : h2 : []
  | otherwise = [g] : h1 : h2 : []
-}
assign_column g (h1 : h2 : t)
  | all (commute_gate g) h1 && all (commute_gate g) h2 =
    h1 : assign_column g (h2 : t)
  | all (commute_gate g) h1 && not (any (overlap_gate g) h1) = (g : h1) : h2 : t
  | all (commute_gate g) h1 = h1a : (g : h1b) : h2 : t
  | otherwise = [g] : h1 : h2 : t
  where
    ol = filter (\x -> wiresOfGate x `intersect` wiresOfGate g /= []) h1
    h1a = ol
    h1b = h1 \\ h1a

assign_column' :: Gate -> [[Gate]] -> [[Gate]]
assign_column' g [] = [[g]]
assign_column' g [h]
  | not (overlap_c_g h g) = [g : h]
  | otherwise = [[g], h]
assign_column' g (h : t)
  | commute_c_g h g = if head t' == [g] then (if overlap_c_g h g then [g] : h : t else (g : h) : t) else h : t'
  | otherwise = [g] : h : t
  where
    t' = assign_column' g t

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
match :: [[Gate]] -> [[Gate]] -> Maybe Subst
match [] _ = Just []
match (h : t) [] = Nothing
match (h : t) sc@(h' : t') = case match_column h h' of
  Nothing -> Nothing
  Just bh -> case match (instantiate bh t) t' of
    Nothing -> match (h : t) sc'
    Just bt -> return $ bh ++ bt
    where
      h'' = h' \\ instantiate_column bh h
      sc' = h'' : t'

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

type Column = [Gate]

instantiate_column :: Subst -> Column -> Column
instantiate_column t c =
  foldl (\c h -> map (instantiate1 h) c) c t

instantiate :: Subst -> [[Gate]] -> [[Gate]]
instantiate [] xs = xs
instantiate (h : t) xs = instantiate t xs'
  where
    xs' = map (map (instantiate1 h)) xs

match_column :: [Gate] -> [Gate] -> Maybe Subst
match_column [] _ = Just []
match_column _ [] = Nothing
match_column (h : t) (h' : t') = case unify_gate h h' of
  Nothing -> match_column (h : t) t'
  Just bh -> do
    bt <- match_column t t'
    return $ bh ++ bt

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
  | i /= i' && j == j' = if i < 0 then Just [(i, i')] else (if i' < 0 then Just [(i', i)] else Nothing)
  | i == i' && j /= j' = if j < 0 then Just [(j, j')] else (if j' < 0 then Just [(j', j)] else Nothing)
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
  | i /= i' && j /= j' && k == k' = unify_gate (CZ j k) (CZ j' k')
  | i /= i' && j /= j' && k == k' = unify_gate (CZ i k) (CZ i' k')
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

type Rule = ([[Gate]], [[Gate]])

type Rules = [Rule]

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
type Rule' = (([[Gate]], [[Gate]]), ([[Gate]], [[Gate]]))

type Rules' = [Rule']

instantiate_rule :: Subst -> Rule' -> Rule'
instantiate_rule b ((ll, lr), (rl, rr)) = ((ll', lr'), (rl', rr'))
  where
    [ll', lr', rl', rr'] = map (instantiate b) [ll, lr, rl, rr]

-- | e.g. this rule ([[CX 1 2],[H 1]], [([H 1], -1)], ([CZ 1 2], 0)])
-- says "CZ 1 2" must be place in the first column, and "H 1" must
-- before "CZ 1 2".
type SqueezedC = [[Gate]]

isH :: Gate -> Bool
isH (H _) = True
isH _ = False

-- | runRules repeatedly
runRules_rep :: Rules' -> SqueezedC -> SqueezedC
runRules_rep rules sc
  | sc == sc' = sc
  | otherwise = runRules_rep rules sc'
  where
    sc' = runRules' rules sc

-- | like runRule, this also makes Squeezed Circuit loosely squeezed.
runRules :: Rules -> SqueezedC -> SqueezedC
runRules t xss =
  foldl (\xss h -> squeeze $ concat $ runRule h xss) xss t

runRules' :: Rules' -> SqueezedC -> SqueezedC
runRules' rules xss = foldl' (flip runRule') xss rules

--runRules' [] xss = xss
--runRules' (h:t) xss = runRules' t (runRule' h xss)

-- | runRule trys to find many matches in a squeezed circuit, and
-- rewrite all of them.

-- | !!!! obsolete !!! Note 1) each rewrite make the squeezed circuit
-- become "loosely squeezed" circuit. 2) after making "loosely
-- squeezed" circuit squeezed again, the redex might appear again, so
-- we need to test whether there are redexes still if we want reduce
-- all redexed.
runRule :: Rule -> [[Gate]] -> [[Gate]]
runRule rule [] = []
runRule rule@(l, r) xss@(h : t) = case match l xss of
  Nothing -> h : runRule rule t
  Just b -> runRule rule $ rewrite (instantiate b l, instantiate b r) xss

runRule' :: Rule' -> [[Gate]] -> [[Gate]]
runRule' rule [] = []
runRule' rule@(l@(ll, lr), r@(rl, rr)) xss@(h : t) = case match ll xss of
  Nothing -> squeeze' h $ runRule' rule t
  Just b -> if null lr' || isNothing (match lr' (drop len xss)) then runRule' rule $ rewrite' rule' xss else squeeze' ha $ runRule' rule xss'
    where
      ha = [head $ head $ fst $ fst rule']
      h' = h \\ ha
      xss' = squeeze' h' t
      len = length ll
      rule' = instantiate_rule b rule
      lr' = snd $ fst rule'

rewrite :: Rule -> [[Gate]] -> [[Gate]]
rewrite (l, r) xss = xss'
  where
    xssl = zipWith (flip (\\)) l xss
    xssr = zipWith (++) r (xssl ++ repeat [])
    xss' = xssr ++ drop (length xssl) xss

rewrite' :: Rule' -> [[Gate]] -> [[Gate]]
rewrite' (l@(ll, lr), r@(rl, rr)) xss = xss'
  where
    xssl = zipWith (flip (\\)) ll xss
    xssr = zipWith (++) rr' (xssl ++ repeat [])
    xss' = squeeze' (concat (rl' ++ xssr)) (drop (length xssl) xss)
    lenl = length xssl
    lenrr = length rr
    rr' = take (max lenl lenrr) $ rr ++ repeat []
    rl' = rl

hrules =
  [ ([[H (-1)], [H (-1)]], [[], []]),
    ([[CX (-1) (-2)], [H (-1)]], [[H (-1)], [CZ (-1) (-2)]]),
    ([[H (-1)], [CX (-1) (-2)]], [[], [CZ (-1) (-2)], [H (-1)]])
    --  ([[H (-1)], [S (-1)], [H (-1)]], [[Z (-1)], [S (-1)], [X (-1)], [H (-1)], [S (-1)]])
  ]

hrules' =
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

adjust_CZ_CCZ (CCZ i j k) = CCZ i' j' k'
  where
    xs = sort [i, j, k]
    i' = head xs
    j' = xs !! 1
    k' = xs !! 2
adjust_CZ_CCZ (CZ i j) = CZ (min i j) (max i j)
adjust_CZ_CCZ x = x

adjust_w = map adjust_CZ_CCZ

apply_rule :: Rule -> [[Gate]] -> [[Gate]]
apply_rule (l, r) scir
  | length l > length scir = scir
  | otherwise = []
  where
    rs = zip l scir
    rs' =
      map
        ( \(xs, ys) ->
            map
              ( \x ->
                  filter (similar_gate x) ys
              )
              xs
        )
        rs

-- | Toffoli to CCX
tof2ccx :: Gate -> Gate
tof2ccx (Toffoli t c1 c2) = CCX t c1 c2
tof2ccx g = g

isCX :: Gate -> Bool
isCX (CX _ _) = True
isCX _ = False

isCCX :: Gate -> Bool
isCCX CCX {} = True
isCCX _ = False

cxccx_rules =
  [ -- similarly, when involving CCX, we need 2 or more repeated rules
    -- considering the symmetry of CCX.
    (([[CX (-1) (-2)], [CCX (-2) (-3) (-4)]], []), ([], [[], [CCX (-2) (-3) (-4)], [CCX (-1) (-3) (-4)], [CX (-1) (-2)]])),
    (([[CX (-1) (-2)], [CCX (-3) (-4) (-5)]], []), ([], [[], [CCX (-3) (-4) (-5)], [CX (-1) (-2)]])),
    (([[CX (-1) (-2)], [CCX (-1) (-3) (-4)]], []), ([], [[], [CCX (-1) (-3) (-4)], [CX (-1) (-2)]]))
  ]

halve_cxccx :: [Gate] -> ([Gate], [Gate])
halve_cxccx xs = (l, r)
  where
    cxs = filter isCX xs
    len' = 13 -- length cxs
    len = if even (length cxs) then length cxs `div` 2 else length cxs `div` 2 + 1 + 1
    lcx = take len cxs
    rcx = drop len cxs
    lccx = takeWhile isCCX xs
    rccx = drop (length lccx + length cxs) xs
    l = lccx ++ lcx
    r = rcx ++ rccx

mv_cxccx :: [Gate] -> ([Gate], [Gate])
mv_cxccx xs = (m, r)
  where
    (m, r) = mv_cxccx_rep (xs, [])

mv_cxccx_rep :: ([Gate], [Gate]) -> ([Gate], [Gate])
mv_cxccx_rep (m, r) = (m', r')
  where
    xss = squeeze m
    xss' = runRules_rep cxccx_rules xss
    rc = last xss'
    (rcx, rm) = partition isCX rc
    (m', r') = if null rcx then (m, r) else mv_cxccx_rep (m'', r'')
    m'' = concat (take (length xss' -1) xss') ++ rm
    r'' = rcx ++ r

mvh :: [Gate] -> ([Gate], [Gate])
mvh xs = (m, r)
  where
    xss = squeeze xs
    xss' = runRules_rep hrules' xss
    rc = last xss'
    (r, rc') = partition isH rc
    m = concat $ take (length xss' - 1) xss' ++ [rc']

show_rule :: Rule' -> [[Gate]]
show_rule ((ll, lr), (rl, rr)) = ll' ++ [[M 0]] ++ rr'
  where
    ll' = map (reindexCir (+ n)) ll
    rr' = map (reindexCir (+ n)) rr
    n = - (minimum $ wiresOfCir $ concat $ ll ++ rr)

print_rule = topdf . show_rule
