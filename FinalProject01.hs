module FinalProject01 where

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List

import CFGParsing
import Data.Time.Format.ISO8601 (yearFormat)

bottomUp :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
bottomUp cfg input =
  let (nts, ts, start, rules) = cfg in
  let startingConfig = ([], input) in
  let goalConfig = ([NoBar start], []) in
  parser [shift, reduce] rules startingConfig goalConfig
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

isRuleCNF :: RewriteRule nt t -> Bool
isRuleCNF rules = case rules of
        (NTRule x y) -> length y == 2
        (TRule x y)  -> True           
        NoRule       -> True

isCNF :: CFG nt t -> Bool
isCNF (nts, ts, start, rules) = all isRuleCNF rules

pathsToGoalFSA :: (Eq st, Eq sy) 
                => ((st,[sy]), [(st,[sy])])     -- (current, history)
                -> [(st,sy,st)]                 -- delta
                -> [(st,[sy])]                  -- getGoalConfigs
                -> [[(st,[sy])]]                -- goals
                
-- (current, history) == config
pathsToGoalFSA (current, history) rules goals =
  undefined

{- ======================== BOTTOM UP ========================
C. Bottom-up: start config -> shift -> reduce -> goal config

        transition      rule            config (stack, input)
0       —               —               (ϵ, the baby saw the boy)       
1       shift           D → the         (D, baby saw the boy)
2       shift           N → baby        (D N, saw the boy)
3       reduce          NP → D N        (NP, saw the boy)
4       shift           V → saw         (NP V, the boy)
5       shift           D → the         (NP V D, boy)
6       shift           N → boy         (NP V D N, ϵ)
7       reduce          NP → D N        (NP V NP, ϵ)
8       reduce          VP → V NP       (NP VP, ϵ)
9       reduce          S → NP VP       (S, ϵ)
-}

-- SHIFT : (Φ  ,xi xi+1 ... xn) 
--        ⇛(ΦA    ,xi+1   ... xn)
-- where there is a rule A → xi in the grammar
-- ie: if rhs terminal (input: xi) can be matched with a lhs nt, then SHIFT and return its nt in rhs (stack: ΦA)
-- config: (ϵ, the baby...) 
--      -> (D, ___ baby...)
shift :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shift r config =
  let (stack, input) = config in
    case input of
      [] -> []
      (x:rest) ->
        let shiftChecker = filter (\rule -> case rule of
                                            TRule nt t -> rhsTRule rule == x) r 
                                            -- if x == rhs terminal -> remove
        in map (\rule -> ParseStep Shift rule                
                                  (stack ++ NoBar (lhs rule) :  -- attach only lhs nt of same r to config
                                  [], rest)) shiftChecker  -- apply to rest

-- REDUCE step: (ΦB1 ...Bm, xi  ... xn) 
--            ⇛ (ΦA       , xi  ... xn)
-- where there is a rule A → B1 ... Bm in the grammar
-- ie: if there's lhs nt (stack: ΦB1 ...Bm), then REDUCE nt lhs from stack and return its rhs nt (stack: ΦA)
-- config: (rhs: D N, input) 
--      -> (lhs: NP , input)
-- let r = rhs nt -> extract -> return it's lhs
-- there's a minor bug here but i cannot seem to fix it
reduce :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
reduce r config =
  let (stack, input) = config in
  let reductionChecker = filter (\rule -> case rule of NTRule nt rhs -> isRHS rule stack) r
  in map (\rule -> ParseStep Reduce rule         -- S -> NP VP
                  (NoBar (lhs rule) :            -- attach lhs nt to config (S -> NP VP)
                  drop (length (rhsNTRule rule)) -- remove rhs of config    (S -> __ __)
                  stack, input)) reductionChecker

isRHS :: Eq nt => RewriteRule nt t -> [Stack nt] -> Bool
isRHS rule stack = 
  let rhs = rhsNTRule rule -- given helper function rhsNTRule from CFGParsing.hs
  in map NoBar rhs == take (length rhs) stack

-- ======================== BOTTOM UP ========================






{- ======================== PARSER ========================
  
-- (parser) take:     a list of transitions, r, startConfig, goalConfig (from bottomUp)
-- (parser) return:   list of all the successfull parses

-- (bottomUp) take:   cfg, input (list of terminal symbols)
-- (bottomUp) return: all possible bottomUp parses of the given list of Ter syms under given CFG
-- * if Ter syms cannot be parsed by CFF == []
-}
parser :: (Eq nt, Eq t)
       => [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]]
          -- ^ List of transition steps. ^                  eg: shift, transition
       -> [RewriteRule nt t]  -- Rules from the CFG.        getRule
       -> Config nt t         -- Starting configuration.    ParseStep NoTransition ??
       -> Config nt t         -- Goal configuration.
       -> [[ParseStep nt t]]  -- List of possible parses.

{- 
******** STUCK: cannot solve ********
- need to keep track of curConfig and history
- find a way to make it to a tuple of two???? -> (curConfig, history)

-> then use case 
-- base case:     curConfig == goalConfig == [[]]
-- rec. case:     curConfig 
--              -> nextConfig = (transitions && r == True)
--              -> repeat until reaching base case

parser transitions r startConfig goalConfig = 
  let (curConfig, history) = startConfig in
    case elem curConfig goalConfig of
      True -> [[]]
      False -> .....
-}
parser transitions r startConfig goalConfig = undefined

-- ======================== PARSER  ========================






{- ======================== TOP DOWN ========================
E. Top-down: start config -> predict -> match -> goal config

        transition      rule            config (stack, input)
0       —               —               (S, the baby saw the boy)
1       predict         S → NP VP       (NP VP, the baby saw the boy)
2       predict         NP → D N        (D N VP, the baby saw the boy)
3       match           D → the         (N VP, baby saw the boy)
4       match           N → baby        (VP, saw the boy)
5       predict         VP → V NP       (V NP, saw the boy)
6       match           V → saw         (NP, the boy)
7       predict         NP → D N        (D N, the boy)
8       match           D → the         (N, boy)
9       match           N → boy         (ϵ, ϵ) 
-}
topDown :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
topDown cfg input =
  let (nts, ts, start, rules) = cfg in
  let startingConfig = ([NoBar start], input) in  -- (A, x1 ...xn)
  let goalConfig = ([], []) in                    -- (empty, empty)
  parser [predict, match] rules startingConfig goalConfig

-- PREDICT step: (AΦ        ,xi ...xn) 
--             ⇛ (B1 ...BmΦ ,xi ...xn)
-- where there is a rule A → B1 ...Bm in the grammar
-- ie: if there's an exist nt in config (stack: A), then PREDICT its rule and return to stack only its rhs nt (stack: B1 ...Bm)
-- (think of a reverse bottom-up reduce step)
-- config: (lhs: S    , input)      S -> NP VP
--      -> (rhs: NP VP, input)
-- extract x (lhs nt) from rule then return its rhs
predict :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
predict r config = 
  let (stack, input) = config in
    case stack of
      [] -> []
      (x:rest) -> case x of
                  NoBar nt ->
                    let predictChecker = filter (\rule -> lhs rule == nt) r
                    in map (\rule -> ParseStep Predict rule
                                    (map NoBar (rhsNTRule rule)
                                    ++ rest, input)) predictChecker

-- match step: (AΦ, xi xi+1 ... xn) 
--           ⇛ (Φ     ,xi+1 ... xn)
-- where there is a rule A → xi in the grammar
-- ie: if there's a matching pair of (A, xi) in config, then MATCH it with r and remove from config
-- (think of a reverse bottom-up shift step)
-- config: (D N, the boy)
--      -> (_ N, ___ boy)
match :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
match r config = 
  let (stack, input) = config in
    case stack of
      [] -> []
      (x:xrest) -> case x of
                  NoBar nt -> case input of
                            [] -> []
                            (y:yrest) -> 
                              let matchChecker = filter (\rule -> case rule of 
                                                                    TRule nt' t -> y == rhsTRule rule
                                                                    NTRule nt' rhs -> nt == lhs rule) r
                              in map (\rule -> ParseStep Match rule (xrest, yrest)) matchChecker
                          
-- ======================== TOP DOWN ========================







{- ======================== LEFT-CORNER ========================
F. Left-corner: start config -> shiftLC -> predictLC -> matchLC -> connectLC -> goal config

        transition      rule            config (stack, input)
0       —               —               (S-bar, the baby saw the boy)
1       shift           D → the         (D S-bar, baby saw the boy)
2       lc-predict      NP → D N        (N-bar NP S-bar, baby saw the boy)
3       match           N → baby        (NP S-bar, saw the boy)
4       lc-connect      S → NP VP       (VP-bar, saw the boy)
5       shift           V → saw         (V VP-bar, the boy)
6       lc-connect      VP → V NP       (NP-bar, the boy)
7       shift           D → the         (D NP-bar, boy)
8       lc-connect      NP → D N        (N-bar, boy)
9       match           N → boy         (ϵ, ϵ)
-}
leftCorner :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
leftCorner cfg input =
  let (nts, ts, start, rules) = cfg in
  let startingConfig = ([Bar start], input) in    -- (A-bar, x1 ...xn)
  let goalConfig = ([], []) in                    -- (empty, empty)
  parser [shiftLC, predictLC, matchLC, connectLC] rules startingConfig goalConfig

-- SHIFT-LC : (Φ      ,xi xi+1 ...xn) 
--          ⇛ (A-barΦ    ,xi+1 ...xn)
-- where there is a rule A→ xi in the grammar
shiftLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shiftLC r config =
  let (stack, input) = config in
    case input of
      [] -> []
      (x:rest) ->
        let shiftChecker = filter (\rule -> case rule of
                                            TRule nt t -> rhsTRule rule == x) r
        in map (\rule -> ParseStep Shift rule                
                                  (NoBar (lhs rule) :        
                                  stack, rest)) shiftChecker

-- PREDICT-LC : (B1Φ              , xi ...xn) 
--            ⇛ (B2-bar...Bm-barAΦ, xi ...xn)
-- where there is a rule A→ B1 ...Bm in the grammar

-- config: (D        S-bar, input)
--      -> (N-bar NP S-bar, input)
-- in config, if x = NoBar TRule is found in rule rhs
--                 -> "predict" rule
--                 -> extract x
--                 -> return stack: rhs excluding x(Bar) ++ lhs (NoBar)
predictLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
predictLC rules (stack, input) = 
  case stack of
    [] -> []
    x:rest -> case x of
                NoBar xs -> 
                  let predictLCChecker = filter (\rule -> case rule of
                                                          NTRule nt rhs -> any (== xs) rhs) rules
                  in map (\rule ->
                            let rhsBars = map Bar (filter (/= xs) (rhsNTRule rule)) 
                                lhsNoBar = NoBar (lhs rule)                        
                            in ParseStep Predict rule 
                               (rhsBars ++ lhsNoBar : rest, input)) predictLCChecker

-- MATCH-LC : AΦ ,xi xi+1 ...xn) 
--         ⇛ (Φ ,    xi+1 ...xn)
-- think of a reverse shift bottom-up
-- config: (N-bar NP S-bar, baby saw the boy)
--      -> (_____ NP S-bar, ____ saw the boy)
matchLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
matchLC r config = 
  let (stack, input) = config in
    case stack of
      [] -> []
      (x:xrest) -> case x of
                  Bar nt -> case input of
                            [] -> []
                            (y:yrest) -> 
                              let matchLCChecker = filter (\rule -> case rule of 
                                                                    TRule nt' t -> y == rhsTRule rule
                                                                    NTRule nt' rhs -> nt == lhs rule) r
                              in map (\rule -> ParseStep Match rule (xrest, yrest)) matchLCChecker
  
-- CONNECT-LC : (B1AΦ     ,xi ...xn) 
--            ⇛ (B2...BmΦ ,xi ...xn)
-- prev config: if there's exists NoBar rhs nt && Bar lhs nt in a rule
-- -> return connect the rule then return it's remaining rule being Bar
-- function compiled but did not return the expected result
connectLC :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
connectLC r config = 
  let (stack, input) = config in
    case stack of
      [] -> []
      (x:rest) -> case x of
                  Bar nt ->
                    let connectBarChecker = filter (\rule -> case rule of
                                                            NTRule lhs rhs -> nt == lhs) r
                    in map (\rule -> 
                      let rhsBar = map NoBar (rhsNTRule rule) in
                      ParseStep Connect rule (rhsBar ++ rest, input)) connectBarChecker

-- ======================== LEFT-CORNER ========================









{- TEST CASES
======================== TOP-DOWN ========================
---------------- LEFT-BRANCHING ----------------
Sentence: the baby saw the boy
** stack does grow in size at most size 3

ghci> predict [(NTRule S [NP,VP])] ([NoBar S], ["the", "baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Predict   S -> NP VP   ([NP,VP],["the","baby","saw","the","boy"])
===== END PARSE =====

ghci> predict [(NTRule NP [D,N])] ([NoBar NP, NoBar VP], ["the", "baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Predict   NP -> D N   ([D,N,VP],["the","baby","saw","the","boy"])
===== END PARSE =====

ghci> match [(TRule D "the")] ([NoBar D, NoBar N, NoBar VP], ["the", "baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Match   D -> "the"   ([N,VP],["baby","saw","the","boy"])
===== END PARSE =====

ghci> match [(TRule N "baby")] ([NoBar N, NoBar VP], ["baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Match   N -> "baby"   ([VP],["saw","the","boy"])
===== END PARSE =====

ghci> predict [(NTRule VP [V,NP])] ([NoBar VP], ["saw", "the", "boy"])
===== BEGIN PARSE =====
Predict   VP -> V NP   ([V,NP],["saw","the","boy"])
===== END PARSE =====

ghci> match [(TRule V "saw")] ([NoBar V, NoBar NP], ["saw", "the", "boy"])
===== BEGIN PARSE =====
Match   V -> "saw"   ([NP],["the","boy"])
===== END PARSE =====



---------------- RIGHT-BRANCHING ----------------
2c) 0 John 1 met 2 the 3 boy 4 that 5 saw 6 the 7 actor 8 that won the award
** does not grow more in size, the biggest stack is at (NP [D,N,SRC])
— The tree structures from [John…the boy] and [that…the actor] are the same. Therefore, parser will continue repeating the same process and the max size for stack is at 3

ghci> predict [(NTRule S [NP,VP])] ([NoBar S], ["John", "met", "the", "boy"])
===== BEGIN PARSE =====
Predict   S -> NP VP   ([NP,VP],["John","met","the","boy"])
===== END PARSE =====

ghci> match [(TRule NP "John")] ([NoBar NP, NoBar VP], ["John", "met", "the", "boy"])
===== BEGIN PARSE =====
Match   NP -> "John"   ([VP],["met","the","boy"])
===== END PARSE =====

ghci> predict [(NTRule VP [V,NP])] ([NoBar VP], ["met","the","boy"])
===== BEGIN PARSE =====
Predict   VP -> V NP   ([V,NP],["met","the","boy"])
===== END PARSE =====

ghci> match [(TRule V "met")] ([NoBar V, NoBar NP], ["met", "the", "boy"])
===== BEGIN PARSE =====
Match   V -> "met"   ([NP],["the","boy"])
===== END PARSE =====

ghci> predict [(NTRule NP [D,N,SRC])] ([NoBar NP], ["the", "boy"])
===== BEGIN PARSE =====
Predict   NP -> D N SRC   ([D,N,SRC],["met","the","boy"])
===== END PARSE =====

---------------- CENTER-EMBEDDING ----------------
3c) 0 the 1 actor 2 the 3 boy 4 the 5 baby saw met won
** stack does grow in size
-> from step 6, stack decreases then increases at its max at step 10

ghci> predict [(NTRule ORC [NP,V])] ([NoBar ORC, NoBar VP], ["the", "boy", "the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Predict   ORC -> NP V   ([NP,V,VP],["the","boy","the","baby","saw","met","won"])
===== END PARSE =====

ghci> predict [(NTRule NP [D,N,ORC])] ([NoBar NP, NoBar V, NoBar VP], ["the", "boy", "the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Predict   NP -> D N ORC   ([D,N,ORC,V,VP],["the","boy","the","baby","saw","met","won"])
===== END PARSE =====

ghci> match [(TRule D "the")] ([NoBar D, NoBar N, NoBar ORC, NoBar V, NoBar VP], ["the", "boy", "the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Match   D -> "the"   ([N,ORC,V,VP],["boy","the","baby","saw","met","won"])
===== END PARSE =====

ghci> match [(TRule N "boy")] ([NoBar N, NoBar ORC, NoBar V, NoBar VP], ["boy", "the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Match   N -> "boy"   ([ORC,V,VP],["the","baby","saw","met","won"])
===== END PARSE =====

ghci> predict [(NTRule ORC [NP,V])] ([NoBar ORC, NoBar V, NoBar VP], ["the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Predict   ORC -> NP V   ([NP,V,V,VP],["the","baby","saw","met","won"])
===== END PARSE =====

predict [(NTRule NP [D,N])] ([NoBar NP, NoBar V, NoBar V, NoBar VP], ["the", "baby", "saw", "met", "won"])

ghci> predict [(NTRule NP [D,N])] ([NoBar NP, NoBar V, NoBar V, NoBar VP], ["the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Predict   NP -> D N   ([D,N,V,V,VP],["the","baby","saw","met","won"])
===== END PARSE =====

======================== BOTTOM-UP ========================
---------------- LEFT-BRANCHING ----------------
1b) Mary’s baby won
** stack doesn’t grow in size

ghci> shift [(TRule NP "Mary")] ([], ["Mary", "'s", "baby", "won"])
===== BEGIN PARSE =====
Shift   NP -> "Mary"   ([NP],["'s","baby","won"])
===== END PARSE =====

ghci> shift [(TRule POSS "'s")] ([NoBar NP], ["'s", "baby", "won"])
===== BEGIN PARSE =====
Shift   POSS -> "'s"   ([NP,POSS],["baby","won"])
===== END PARSE =====

ghci> shift [(TRule N "baby")] ([NoBar NP, NoBar POSS], ["baby", "won"])
===== BEGIN PARSE =====
Shift   N -> "baby"   ([NP,POSS,N],["won"])
===== END PARSE =====
  
ghci> shift [(TRule V "won")] ([NoBar NP, NoBar POSS, NoBar N], ["won"])
===== BEGIN PARSE =====
Shift   V -> "won"   ([NP,POSS,N,V],[])
===== END PARSE =====
  
ghci> reduce [(NTRule S [NP,VP])] ([NoBar NP, NoBar V], [])
===== BEGIN PARSE =====
===== END PARSE =====  

---------------- RIGHT-BRANCHING ----------------
2b) the baby saw the boy
** does grow in size -> until keeps increasing when until there’s no more input left, after that it will reduce == decrease

ghci> shift [(TRule D "the")] ([], ["the", "baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Shift   D -> "the"   ([D],["baby","saw","the","boy"])
===== END PARSE =====

ghci> shift [(TRule N "baby")] ([NoBar D], ["baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Shift   N -> "baby"   ([D,N],["saw","the","boy"])
===== END PARSE =====

ghci> reduce [(NTRule NP [D,N])]  ([NoBar D, NoBar N], ["saw", "the", "boy"])
===== BEGIN PARSE =====
Reduce   NP -> D N   ([NP],["saw","the","boy"])
===== END PARSE =====

ghci> shift [(TRule V "saw")] ([NoBar NP], ["saw","the","boy"])
===== BEGIN PARSE =====
Shift   V -> "saw"   ([NP,V],["the","boy"])
===== END PARSE =====

ghci> shift [(TRule D "the")] ([NoBar NP, NoBar V], ["the", "boy"])
===== BEGIN PARSE =====
Shift   D -> "the"   ([NP,V,D],["boy"])
===== END PARSE =====

ghci> shift [(TRule N "boy")] ([NoBar NP, NoBar V, NoBar D], ["boy"])
===== BEGIN PARSE =====
Shift   N -> "boy"   ([NP,V,D,N],[])
===== END PARSE =====

---------------- CENTER-BRANCHING ----------------
3c) the actor the boy the baby saw met won
** stack grows in size

ghci> shift [(TRule N "boy")] ([NoBar D, NoBar N, NoBar D], ["boy", "the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Shift   N -> "boy"   ([D,N,D,N],["the","baby","saw","met","won"])
===== END PARSE =====

ghci> shift [(TRule D "the")] ([NoBar D, NoBar N, NoBar D, NoBar N], ["the", "baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Shift   D -> "the"   ([D,N,D,N,D],["baby","saw","met","won"])
===== END PARSE =====

ghci> shift [(TRule N "baby")] ([NoBar D, NoBar N, NoBar D, NoBar N, NoBar D], ["baby", "saw", "met", "won"])
===== BEGIN PARSE =====
Shift   N -> "baby"   ([D,N,D,N,D,N],["saw","met","won"])

** should be [D, N, D, N, NP] instead of [NP, D, N, D, N]
ghci> reduce [(NTRule NP [D,N])] ([NoBar D, NoBar N, NoBar D, NoBar N, NoBar D, NoBar N], ["saw", "met", "won"])
===== BEGIN PARSE =====
Reduce   NP -> D N   ([NP,D,N,D,N],["saw","met","won"])
===== END PARSE =====

ghci> shift [(TRule V "met")] ([NoBar D, NoBar N, NoBar D, NoBar N, NoBar NP], ["saw", "met", "won"])
===== BEGIN PARSE =====
===== END PARSE =====

======================== LEFT-CORNER ========================
---------------- RIGHT-BRANCHING ----------------
example is from w8 handout: the baby saw the boy

ghci> shiftLC ([TRule D "the"]) ([Bar S], ["the", "baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Shift   D -> "the"   ([D,S*],["baby","saw","the","boy"])
===== END PARSE =====
  
ghci>  predictLC [(NTRule NP [D,N])] ([NoBar D, Bar S], ["baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Predict   NP -> D N   ([N*,NP,S*],["baby","saw","the","boy"])
===== END PARSE =====

ghci> matchLC [(TRule N "baby")] ([Bar N, NoBar NP, Bar S], ["baby", "saw", "the", "boy"])
===== BEGIN PARSE =====
Match   N -> "baby"   ([NP,S*],["saw","the","boy"])
===== END PARSE =====

**** FAIL ****
ghci> connectLC [(NTRule S [NP, VP])] ([NoBar NP, Bar S], ["saw", "the", "baby"])
===== BEGIN PARSE =====
===== END PARSE =====


---------------- CENTER-BRANCHING ----------------
3c) the actor the boy the baby saw met won 
** does grow in size

ghci> matchLC [(TRule N "boy")] ([Bar N, Bar ORC, NoBar NP, Bar ORC, NoBar NP, Bar S], ["boy", "the", "baby", "saw"])
===== BEGIN PARSE =====
Match   N -> "boy"   ([ORC*,NP,ORC*,NP,S*],["the","baby","saw"])
===== END PARSE =====

ghci> shiftLC ([TRule D "the"]) ([Bar ORC, NoBar NP, Bar ORC, NoBar NP, Bar S], ["the", "baby", "saw"])
===== BEGIN PARSE =====
Shift   D -> "the"   ([D,ORC*,NP,ORC*,NP,S*],["baby","saw"])
===== END PARSE =====

ghci> predictLC [(NTRule NP [D,N])] ([NoBar D, Bar ORC, NoBar NP, Bar ORC, NoBar NP, Bar S], ["baby", "saw"])
===== BEGIN PARSE =====
Predict   NP -> D N   ([N*,NP,ORC*,NP,ORC*,NP,S*],["baby","saw"])
===== END PARSE =====

ghci> matchLC [(TRule N "baby")] ([Bar N, NoBar NP, Bar ORC, NoBar NP, Bar ORC, NoBar NP, Bar S], ["baby", "saw"])
===== BEGIN PARSE =====
Match   N -> "baby"   ([NP,ORC*,NP,ORC*,NP,S*],["saw"])
===== END PARSE =====

**** FAIL ****
ghci> connectLC [(NTRule ORC [NP,V])] ([NoBar NP, Bar ORC, NoBar NP, Bar ORC, NoBar NP, Bar S], ["saw"])
===== BEGIN PARSE =====
*** Exception: FinalProject01.hs:(374,19)-(380,87): Non-exhaustive patterns in case
-}