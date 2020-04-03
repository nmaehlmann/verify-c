{-# LANGUAGE GADTs #-}
module VC (VC(..), VCInfo(..), verifyProgram, unliftMemory) where
import AST
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader
import Logic.Simplify
import qualified Data.Set as Set
import Logic.Lift
import Memory.Lift
import qualified Memory.Unlift
import Replace.State
import Replace.AExp
import Logic.FO

data VC a = VC VCInfo (BExp FO a) deriving Show

data VCInfo 
    = CWhileTrue LineNo 
    | CWhileFalse LineNo 
    | CAssertion LineNo 
    | CFunCall Idt LineNo
    | CPrecondition Idt
    deriving Show

type VerifyC = Reader Ctx

data Ctx = Ctx { functionMap :: Map Idt FunctionDefinition }

vcSimplify :: VC Refs -> VC Refs
vcSimplify (VC info vc) = VC info $ simplify vc

unliftMemory :: VC Refs -> Maybe (VC Plain)
unliftMemory (VC info vc) = VC info <$> Memory.Unlift.unliftMemory vc

verifyFunction :: FunctionDefinition -> VerifyC [VC Refs]
verifyFunction f = do
    let precondition = liftMemory $ funDefPrecond f
    let postcondition = liftMemory $ funDefPostcond f
    let name = funDefName f
    let localArgs = Set.fromList $ fmap (LIdt . idtFromDecl) $ funDefArgs f
    awpBody <- simplifyLocalVars localArgs <$> awp (funDefBody f) postcondition postcondition
    wvcs <- wvc (funDefBody f) BFalse postcondition
    let preconditionVC = VC (CPrecondition name) $ BBinExp Implies precondition awpBody
    return $ map vcSimplify $ preconditionVC : wvcs

verifyProgram :: Program -> [VC Refs]
verifyProgram program@(Program functions) = 
    let ctx = generateContext program
    in  concat $ runReader (sequence (map verifyFunction functions)) ctx

generateContext :: Program -> Ctx
generateContext (Program fs) = Ctx {functionMap = functionMap'}
    where functionMap' = generateContext' fs

generateContext' :: [FunctionDefinition] -> Map Idt FunctionDefinition
generateContext' [] = Map.empty
generateContext' (f:fs) = 
    let insert = Map.insert (funDefName f)
        prevCtx = generateContext' fs
    in  insert f prevCtx

awp, awp' :: Stmt -> BExpFO -> BExpFO -> VerifyC BExpFO
awp s q qr = simplify <$> awp' s q qr
awp' Empty q _ = return q
awp' (Assertion q _) _ _ = return $ liftMemory q
awp' (ITE condition sTrue sFalse) q qr = do
    awpTrue <- awp sTrue q qr
    awpFalse <- awp sFalse q qr
    let foCondition = liftMemory $ liftLogic condition
    let foTrue  = BBinExp And foCondition awpTrue
    let foFalse = BBinExp And (BNeg foCondition) awpFalse
    return $ BBinExp Or foTrue foFalse
awp' (While _ inv _ _) _ _ = return $ liftMemory inv
awp' (Seq s1 s2) q qr = awp s2 q qr >>= \awpS2 -> awp s1 awpS2 qr
awp' (Assignment idt aExp) q _ = 
    let newState = Update sigma (dagger (lLiftLogic idt)) (hashmark (aLiftLogic aExp))
        oldState = sigma
    in  return $ replaceState oldState newState q
awp' (Declaration idt) q _ = return $ simplifyLocalVars (Set.singleton (LIdt idt)) q
awp' (Return Nothing) _ qr = return qr
awp' (Return (Just e)) _ qr = return $ replaceAExp resultLExp (hashmark (aLiftLogic e)) qr
awp' (FunCall _ funName suppliedArgs _) _ _ = do
    calledFunction <- lookupFunction funName
    let funPrecond = liftMemory $ funDefPrecond calledFunction
    let readIdt i = LRead $ ReadLExp sigma $ LIdt i
    let funArgs = fmap readIdt $ fmap idtFromDecl $ funDefArgs calledFunction
    let suppliedArgsRefs = fmap (hashmark . aLiftLogic) suppliedArgs
    let replacements = zip funArgs suppliedArgsRefs
    let replacedPrecondition = foldl replace funPrecond replacements
    return replacedPrecondition

lookupFunction :: Idt -> VerifyC FunctionDefinition
lookupFunction i = do
    funMap <- functionMap <$> ask
    return $ funMap Map.! i

wvc :: Stmt -> BExpFO -> BExpFO -> VerifyC [VC Refs]
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
wvc (While cond inv body line) q qr = do
    let foCond = liftMemory $ liftLogic cond
    let foInv = liftMemory inv
    let foInvAndCond = BBinExp And foInv foCond
    let foInvAndNotCond = BBinExp And foInv $ BNeg foCond
    awpBody <- awp body foInv qr
    wvcBody <- wvc body foInv qr
    let vcWhileTrue = VC (CWhileTrue line) $ BBinExp Implies foInvAndCond awpBody
    let vcWhileFalse = VC (CWhileFalse line) $ BBinExp Implies foInvAndNotCond q
    return $ vcWhileTrue
        : vcWhileFalse
        : wvcBody
wvc (Assertion fo line) q _ = return $ return $ VC (CAssertion line) $ BBinExp Implies (liftMemory fo) q
wvc (Return _) _ _ = return []
wvc (FunCall maybeAssignment funName suppliedArgs line) q _ = do
    calledFunction <- lookupFunction funName
    let funPostcond = liftMemory $ funDefPostcond calledFunction
    let readIdt i = LRead $ ReadLExp sigma $ LIdt i
    let funArgs = fmap readIdt $ fmap idtFromDecl $ funDefArgs calledFunction
    let suppliedArgsRefs = fmap (hashmark . aLiftLogic) suppliedArgs
    let resultReplace = case maybeAssignment of
            (Just assignTo) ->
                let aResult = resultLExp
                    aTarget = hashmark $ aLiftLogic $ AIdt $ assignTo
                in  [(aResult, aTarget)]
            Nothing -> []
    let replacements = zip funArgs suppliedArgsRefs
    let replacedPostcondition = foldl replace funPostcond (resultReplace ++ replacements)
    return $ return $ VC (CFunCall funName line) $ BBinExp Implies replacedPostcondition q

replace :: BExp FO Refs -> (LExp FO Refs, AExp FO Refs) -> BExp FO Refs
replace fo (toReplace, replaceWith) = replaceAExp toReplace replaceWith fo

idtFromDecl :: Decl -> Idt
idtFromDecl (Decl _ idt) = idt