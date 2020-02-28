module VC where

import AST

-- verificate :: FunctionDefinition -> [FOSExp]
-- verificate f = []

awp :: Stmt -> FOSExp -> FOSExp
awp Empty p = p
awp (Assertion p) _ = stateifyFO p
awp (ITE condition sTrue sFalse) p = 
    let awpTrue = awp sTrue p
        awpFalse = awp sFalse p
        foCondition = stateifyFO $ bExpToFOExp condition
        foTrue  = FOBinExp And foCondition awpTrue
        foFalse = FOBinExp And (FONeg foCondition) awpFalse
    in FOBinExp Or foTrue foFalse
awp (While _ inv _) _ = stateifyFO inv
awp (Seq s1 s2) p = awp s1 $ awp s2 p
awp (Assignment idt aExp) p = 
    let newState = Update sigma (dagger idt) (hashmark aExp)
        oldState = sigma
    in  replaceState oldState newState p
awp (Return _) _ = error "unsupported yet"

wvc :: Stmt -> FOSExp -> [FOSExp]
wvc Empty _ = []
wvc (Assignment _ _ ) _ = []
wvc (Seq s1 s2) p = wvc s1 (awp s2 p) ++ wvc s2 p
wvc (ITE _ sTrue sFalse) p = wvc sTrue p ++ wvc sFalse p
wvc (While cond inv body) p =
    let foCond = stateifyFO $ bExpToFOExp cond
        foInv = stateifyFO inv
        foInvAndCond = FOBinExp And foInv foCond
        foInvAndNotCond = FOBinExp And foInv $ FONeg foCond
    in  FOBinExp Implies foInvAndCond (awp body foInv)
        : FOBinExp Implies foInvAndNotCond p
        : wvc body foInv
wvc (Assertion fo) p = [FOBinExp Implies (stateifyFO fo) p]
wvc (Return _) _ = []

-- p[aExp/idt]
-- Q[upd(s,idt † ,aExp # )/s]
-- awp (Assignment idt aExp) p = 

replaceState :: State -> State -> FOSExp -> FOSExp
replaceState sOld sNew expOld = fmap (replaceStateInASExp sOld sNew) expOld

replaceStateInASExp :: State -> State -> ASExp -> ASExp
replaceStateInASExp _ _ (ASLit i) = ASLit i
replaceStateInASExp sOld sNew (ASRead readLExp) = ASRead $ replaceStateInRead sOld sNew readLExp
replaceStateInASExp sOld sNew (ASBinExp op l r) = ASBinExp op (replaceStateInASExp sOld sNew l) (replaceStateInASExp sOld sNew r)
replaceStateInASExp sOld sNew (ASArray fields) = ASArray $ map (replaceStateInASExp sOld sNew) fields
replaceStateInASExp sOld sNew (ASFunCall name args) = ASFunCall name $ map (replaceStateInASExp sOld sNew) args

replaceStateInRead :: State -> State -> ReadLExp -> ReadLExp
replaceStateInRead sOld sNew (ReadLExp sNested lSExp) = ReadLExp (replaceStateInNestedState sOld sNew sNested) (replaceStateInLSExp sOld sNew lSExp)

replaceStateInNestedState :: State -> State -> State -> State
replaceStateInNestedState sOld sNew sNested | sOld == sNested = sNew
replaceStateInNestedState sOld sNew (Update state lSExp aSExp) = Update 
    (replaceStateInNestedState sOld sNew state) 
    (replaceStateInLSExp sOld sNew lSExp)
    (replaceStateInASExp sOld sNew aSExp)
replaceStateInNestedState _ _ s = s

replaceStateInLSExp :: State -> State -> LSExp -> LSExp
replaceStateInLSExp _ _ (LSIdt idt) = LSIdt idt
replaceStateInLSExp sOld sNew (LSArray lSExp aSExp) = LSArray (replaceStateInLSExp sOld sNew lSExp) (replaceStateInASExp sOld sNew aSExp)
replaceStateInLSExp sOld sNew (LSStructPart lSExp idt) = LSStructPart (replaceStateInLSExp sOld sNew lSExp) idt
replaceStateInLSExp sOld sNew (LSRead readLExp) = LSRead $ replaceStateInRead sOld sNew readLExp

bExpToFOExp :: BExp -> FOExp
bExpToFOExp BTrue = FOTrue
bExpToFOExp BFalse = FOFalse
bExpToFOExp (BComp op l r) = FOComp op l r
bExpToFOExp (BNeg b) = FONeg $ bExpToFOExp b
bExpToFOExp (BBinExp op l r) = FOBinExp op (bExpToFOExp l) (bExpToFOExp r)

stateifyFO :: FOExp -> FOSExp
stateifyFO = fmap stateifyAExp

stateifyAExp :: AExp -> ASExp
stateifyAExp = hashmark

dagger :: LExp -> LSExp
dagger (LIdt idt) = LSIdt idt
dagger (LArray idt idx) = LSArray (dagger idt) (hashmark idx)
dagger (LStructPart struct part) = LSStructPart (dagger struct) part
dagger (LDereference idt) = LSRead $ ReadLExp sigma $ dagger idt

hashmark :: AExp -> ASExp
hashmark (ALit i) = ASLit i
hashmark (AIdt l) = ASRead $ ReadLExp sigma $ dagger l
hashmark (ABinExp op l r) = ASBinExp op (hashmark l) (hashmark r)
hashmark (AArray fields) = ASArray $ map hashmark fields
hashmark (AFunCall name args) = ASFunCall name $ map hashmark args

sigma :: State
sigma = Atomic "s"