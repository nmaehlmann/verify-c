{-# LANGUAGE GADTs #-}
module Logic.Simplify (simplify, simplifyLocalVars, simplifyAExpFO) where
import AST
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import Logic.Simplified
import Logic.FO
import Memory.Eq

findInequalities :: BExpFO -> Set Inequality
findInequalities (BComp NotEqual (AIdt l1) (AIdt l2)) = Set.singleton $ notEqual l1 l2
findInequalities (BBinExp And fo1 fo2) = Set.union (findInequalities fo1) (findInequalities fo2)
findInequalities _ = Set.empty

simplify :: BExpFO -> BExpFO
simplify = simplifyLocalVars Set.empty

simplifyLocalVars :: Set LExpFO -> BExpFO -> BExpFO
simplifyLocalVars locals a =
    let ctx = SimplificationCtx 
            { inequalities = Set.empty
            , localVars = locals
            }
    in  case runReaderT (simplifyBExpFO a) ctx of
            Updated updated -> simplifyLocalVars locals updated
            Unchanged unchanged -> unchanged

addInequalities :: Set Inequality -> SimplificationCtx -> SimplificationCtx
addInequalities ineqs ctx = ctx {inequalities = Set.union ineqs $ inequalities ctx}

simplifyBExpFO :: BExpFO -> Simplified BExpFO
simplifyBExpFO BTrue = return BTrue
simplifyBExpFO BFalse = return BFalse
simplifyBExpFO (BComp op l r) = do
    updatedL <- simplifyAExpFO l
    updatedR <- simplifyAExpFO r
    return $ BComp op updatedL updatedR
simplifyBExpFO (BNeg fo) = BNeg <$> simplifyBExpFO fo
simplifyBExpFO (BBinExp op l r) = do
    updatedL <- simplifyBExpFO l
    let lhsInequalities = if op == Implies 
            then findInequalities updatedL 
            else Set.empty
    updatedR <- local (addInequalities lhsInequalities) $ simplifyBExpFO r
    return $ BBinExp op updatedL updatedR
simplifyBExpFO (BForall i fo) = BForall i <$> simplifyBExpFO fo
simplifyBExpFO (BExists i fo) = BExists i <$> simplifyBExpFO fo
simplifyBExpFO (BPredicate i fos) = BPredicate i <$> mapM simplifyAExpFO fos 

simplifyAExpFO :: AExpFO -> Simplified AExpFO
simplifyAExpFO original@(AIdt (LRead (Update state toUpdate aExp) toRead)) = do
    memComparison <- compareLExp toRead toUpdate
    case memComparison of
        MemEq -> update aExp
        _ -> simplifyAExpFO' original
simplifyAExpFO a = simplifyAExpFO' a
        
simplifyAExpFO' :: AExpFO -> Simplified AExpFO
simplifyAExpFO' (ALit a) = return $ ALit a
simplifyAExpFO' (ALogVar v) = return $ ALogVar v
simplifyAExpFO' (ABinExp op l r) = do
    updatedL <- simplifyAExpFO l
    updatedR <- simplifyAExpFO r
    return $ ABinExp op updatedL updatedR
simplifyAExpFO' (AFunCall funName funArgs) = AFunCall funName <$> mapM simplifyAExpFO funArgs
simplifyAExpFO' (AIdt lExp) = AIdt <$> simplifyLExp lExp

simplifyLExp :: LExpFO -> Simplified LExpFO
simplifyLExp original@(LRead (Update state toUpdate _) toRead) = do
    memComparison <- compareLExp toRead toUpdate
    case memComparison of
        MemNotEq -> update $ LRead state toRead 
        _ -> simplifyLExp' original
simplifyLExp l = simplifyLExp' l

simplifyLExp' :: LExpFO -> Simplified LExpFO
simplifyLExp' (LIdt i) = return $ LIdt i
simplifyLExp' (LArray name idx) = do
    newName <- simplifyLExp name
    newIdx <- simplifyAExpFO idx
    return $ LArray newName newIdx
simplifyLExp' (LStructurePart struct part) = do
    newStruct <- simplifyLExp struct
    return $ LStructurePart newStruct part
simplifyLExp' (LRead state toRead) = 
    LRead <$> simplifyState state <*> simplifyLExp toRead

simplifyState :: State -> Simplified State
simplifyState original@(Update (Update s l1 _) l2 w) = do
    memComparison <- compareLExp l1 l2
    case memComparison of
        MemEq -> update $ Update s l2 w
        _ -> simplifyState' original
simplifyState s = simplifyState' s

simplifyState' :: State -> Simplified State
simplifyState' (Atomic name) = return $ Atomic name
simplifyState' (Update state lExp aExp) =
    Update <$> simplifyState state <*> simplifyLExp lExp <*> simplifyAExpFO aExp