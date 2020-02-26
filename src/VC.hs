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
dagger = fmap hashmark

hashmark :: AExp -> ASExp
hashmark = fmap f
    where f (ReadLExp x) = (SReadLExp (Atomic "s") (dagger x))