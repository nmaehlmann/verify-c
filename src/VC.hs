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
import UnliftMemory
import ReplaceState
import ReplaceAExp
import FOTypes

verifyFunction :: FunctionDefinition -> VC [BExpFO]
verifyFunction f = do
    let precondition = bLiftMemory $ funDefPrecond f
    let postcondition = bLiftMemory $ funDefPostcond f
    awpBody <- awp (funDefBody f) postcondition postcondition
    wvcs <- wvc (funDefBody f) BFalse postcondition
    return $ map simplify $ BBinExp Implies precondition awpBody : wvcs

verifyProgram :: Program -> Either [BExpFO] [BExp FO Plain]
verifyProgram program@(Program functions) = 
    let ctx = generateContext program
        vcs = concat $ runReader (sequence (map verifyFunction functions)) ctx
    in  case mapM bUnliftMemory vcs of
        (Just plainVCs) -> Right plainVCs
        Nothing         -> Left vcs

type VC = Reader Ctx

data Ctx = Ctx 
    { functionMap :: Map Idt FunctionDefinition
    }

emptyCtx :: Ctx
emptyCtx = Ctx 
    { functionMap = Map.empty
    }

generateContext :: Program -> Ctx
generateContext (Program fs) = Ctx {functionMap = functionMap'}
    where functionMap' = generateContext' fs

generateContext' :: [FunctionDefinition] -> Map Idt FunctionDefinition
generateContext' [] = Map.empty
generateContext' (f:fs) = 
    let insert = Map.insert (funDefName f)
        prevCtx = generateContext' fs
    in  insert f prevCtx

awp :: Stmt -> BExpFO -> BExpFO -> VC BExpFO
awp Empty q _ = return q
awp (Assertion q) _ _ = return $ bLiftMemory q
awp (ITE condition sTrue sFalse) q qr = do
    awpTrue <- awp sTrue q qr
    awpFalse <- awp sFalse q qr
    let foCondition = bLiftMemory $ bLiftLogic condition
    let foTrue  = BBinExp And foCondition awpTrue
    let foFalse = BBinExp And (BNeg foCondition) awpFalse
    return $ BBinExp Or foTrue foFalse
awp (While _ inv _) _ _ = return $ bLiftMemory inv
awp (Seq s1 s2) q qr = awp s2 q qr >>= \awpS2 -> awp s1 awpS2 qr
awp (Assignment idt aExp) q _ = 
    let newState = Update sigma (dagger (lLiftLogic idt)) (hashmark (aLiftLogic aExp))
        oldState = sigma
    in  return $ bReplaceState oldState newState q
awp (Declaration idt) q _ = return $ simplifyLocalVars (Set.singleton (dagger (lLiftLogic idt))) q
awp (Return Nothing) _ qr = return qr
awp (Return (Just e)) _ qr = return $ bReplaceAExp (hashmark (AIdt resultLExp)) (hashmark (aLiftLogic e)) qr
awp (FunCall _ funName suppliedArgs) _ _ = do
    calledFunction <- lookupFunction funName
    let funPrecond = bLiftMemory $ funDefPrecond calledFunction
    let funArgs = fmap (hashmark . AIdt . LIdt . idtFromDecl) $ funDefArgs calledFunction
    let suppliedArgsRefs = fmap (hashmark . aLiftLogic) suppliedArgs
    let replacements = zip funArgs suppliedArgsRefs
    let replacedPrecondition = foldl replace funPrecond replacements
    return replacedPrecondition

lookupFunction :: Idt -> VC FunctionDefinition
lookupFunction i = do
    funMap <- functionMap <$> ask
    return $ funMap Map.! i

wvc :: Stmt -> BExpFO -> BExpFO -> VC [BExpFO]
wvc Empty _ _ = return []
wvc (Assignment _ _) _ _ = return []
wvc (Declaration _)  _ _ = return []
wvc (Seq s1 s2) q qr = do
    awpS2 <- awp s2 q qr
    wvcS1 <- wvc s1 awpS2 qr
    wvcS2 <- wvc s2 q qr
    return $ wvcS1 ++ wvcS2
wvc (ITE _ sTrue sFalse) q qr = do
    wvcCaseTrue <- wvc sTrue q qr
    wvcCaseFalse <- wvc sFalse q qr
    return $ wvcCaseTrue ++ wvcCaseFalse
wvc (While cond inv body) q qr = do
    let foCond = bLiftMemory $ bLiftLogic cond
    let foInv = bLiftMemory inv
    let foInvAndCond = BBinExp And foInv foCond
    let foInvAndNotCond = BBinExp And foInv $ BNeg foCond
    awpBody <- awp body foInv qr
    wvcBody <- wvc body foInv qr
    return $ BBinExp Implies foInvAndCond awpBody
        : BBinExp Implies foInvAndNotCond q
        : wvcBody
wvc (Assertion fo) q _ = return [BBinExp Implies (bLiftMemory fo) q]
wvc (Return _) _ _ = return []
wvc (FunCall maybeAssignment funName suppliedArgs) q _ = do
    calledFunction <- lookupFunction funName
    let funPostcond = bLiftMemory $ funDefPostcond calledFunction
    let funArgs = fmap (hashmark . AIdt . LIdt . idtFromDecl) $ funDefArgs calledFunction
    let suppliedArgsRefs = fmap (hashmark . aLiftLogic) suppliedArgs
    let resultReplace = case maybeAssignment of
            (Just assignTo) ->
                let aResult = hashmark $ AIdt resultLExp
                    aTarget = hashmark $ aLiftLogic $ AIdt $ assignTo
                in  [(aResult, aTarget)]
            Nothing -> []
    let replacements = zip funArgs suppliedArgsRefs
    let replacedPostcondition = foldl replace funPostcond (resultReplace ++ replacements)
    return $ [BBinExp Implies replacedPostcondition q]

replace :: BExp FO Refs -> (AExp FO Refs, AExp FO Refs) -> BExp FO Refs
replace fo (toReplace, replaceWith) = bReplaceAExp toReplace replaceWith fo

idtFromDecl :: Decl -> Idt
idtFromDecl (Decl _ idt) = idt