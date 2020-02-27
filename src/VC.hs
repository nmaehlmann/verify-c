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

-- p[aExp/idt]
-- Q[upd(s,idt † ,aExp # )/s]
-- awp (Assignment idt aExp) p = 


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

