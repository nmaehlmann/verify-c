{-# LANGUAGE GADTs #-}
module VC where

import AST

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Simplification
import qualified Data.Set as Set
import LiftLogic
import LiftMemory
import ReplaceState

verify :: FunctionDefinition -> [BExpFO]
verify f = map simplify $ BBinExp Implies precondition awpBody : wvcs
    where
        awpBody = awp (funDefBody f) postcondition postcondition
        precondition = bLiftMemory $ funDefPrecond f
        postcondition = bLiftMemory $ funDefPostcond f
        wvcs = wvc (funDefBody f) BFalse postcondition

type VC = Reader Ctx

data Ctx = Ctx 
    { preconditions :: Map Idt BExpFO
    , postconditions :: Map Idt BExpFO
    }

generateContext :: Program -> Ctx
generateContext (Program fs) = Ctx {preconditions = pres, postconditions = posts}
    where (pres, posts) = generateContext' fs

generateContext' :: [FunctionDefinition] -> (Map Idt BExpFO, Map Idt BExpFO)
generateContext' [] = (Map.empty, Map.empty)
generateContext' (f:fs) = 
    let insert = Map.insert (funDefName f)
        pre = bLiftMemory $ funDefPrecond f
        post = bLiftMemory $ funDefPostcond f
        (prevPre, prevPost) = generateContext' fs
    in  (insert pre prevPre, insert post prevPost)

awp :: Stmt -> BExpFO -> BExpFO -> BExpFO
awp Empty q _ = q
awp (Assertion q) _ _ = bLiftMemory q
awp (ITE condition sTrue sFalse) q qr = 
    let awpTrue = awp sTrue q qr
        awpFalse = awp sFalse q qr
        foCondition = bLiftMemory $ bLiftLogic condition
        foTrue  = BBinExp And foCondition awpTrue
        foFalse = BBinExp And (BNeg foCondition) awpFalse
    in BBinExp Or foTrue foFalse
awp (While _ inv _) _ _ = bLiftMemory inv
awp (Seq s1 s2) q qr = awp s1 (awp s2 q qr) qr
awp (Assignment idt aExp) q _ = 
    let newState = Update sigma (dagger (lLiftLogic idt)) (hashmark (aLiftLogic aExp))
        oldState = sigma
    in  bReplaceState oldState newState q
awp (Declaration idt) q _ = simplifyLocalVars (Set.singleton (dagger (lLiftLogic idt))) q
awp (Return Nothing) _ qr = qr
awp (Return _) _ _ = error "unsupported yet"
awp (FunCall _ _ _) _ _ = error "unsupported yet"

wvc :: Stmt -> BExpFO -> BExpFO -> [BExpFO]
wvc Empty _ _ = []
wvc (Assignment _ _) _ _ = []
wvc (Declaration _)  _ _ = []
wvc (Seq s1 s2) q qr = wvc s1 (awp s2 q qr) qr ++ wvc s2 q qr
wvc (ITE _ sTrue sFalse) q qr = wvc sTrue q qr ++ wvc sFalse q qr
wvc (While cond inv body) q qr =
    let foCond = bLiftMemory $ bLiftLogic cond
        foInv = bLiftMemory inv
        foInvAndCond = BBinExp And foInv foCond
        foInvAndNotCond = BBinExp And foInv $ BNeg foCond
    in  BBinExp Implies foInvAndCond (awp body foInv qr)
        : BBinExp Implies foInvAndNotCond q
        : wvc body foInv qr
wvc (Assertion fo) q _ = [BBinExp Implies (bLiftMemory fo) q]
wvc (Return _) _ _ = []
wvc (FunCall _ _ _) _ _ = error "unsupported yet"