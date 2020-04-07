{-# LANGUAGE GADTs #-}
module Logic.Simplify (simplify, simplifyLocalVars, simplifyAExp) where
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
    in  case runReaderT (simplifyBExp a) ctx of
            Updated updated -> simplifyLocalVars locals updated
            Unchanged unchanged -> unchanged

addInequalities :: Set Inequality -> SimplificationCtx -> SimplificationCtx
addInequalities ineqs ctx = ctx {inequalities = Set.union ineqs $ inequalities ctx}

simplifyBExp :: BExpFO -> Simplified BExpFO
simplifyBExp BTrue = return BTrue
simplifyBExp BFalse = return BFalse
simplifyBExp (BComp op l r) = do
    updatedL <- simplifyAExp l
    updatedR <- simplifyAExp r
    return $ BComp op updatedL updatedR
simplifyBExp (BNeg fo) = BNeg <$> simplifyBExp fo
simplifyBExp (BBinExp op l r) = do
    updatedL <- simplifyBExp l
    let lhsInequalities = if op == Implies 
            then findInequalities updatedL 
            else Set.empty
    updatedR <- local (addInequalities lhsInequalities) $ simplifyBExp r
    return $ BBinExp op updatedL updatedR
simplifyBExp (BForall i fo) = BForall i <$> simplifyBExp fo
simplifyBExp (BExists i fo) = BExists i <$> simplifyBExp fo
simplifyBExp (BPredicate i fos) = BPredicate i <$> mapM simplifyAExp fos 

simplifyAExp :: AExpFO -> Simplified AExpFO
simplifyAExp original@(AIdt (LRead (Update state toUpdate aExp) toRead)) = do
    memComparison <- compareLExp toRead toUpdate
    case memComparison of
        MemEq -> update aExp
        _ -> simplifyAExp' original
simplifyAExp a = simplifyAExp' a
        
simplifyAExp' :: AExpFO -> Simplified AExpFO
simplifyAExp' (ALit a) = return $ ALit a
simplifyAExp' (ALogVar v) = return $ ALogVar v
simplifyAExp' (ABinExp op l r) = ABinExp op <$> simplifyAExp l <*> simplifyAExp r
simplifyAExp' (AFunCall funName funArgs) = AFunCall funName <$> mapM simplifyAExp funArgs
simplifyAExp' (AIdt lExp) = AIdt <$> simplifyLExp lExp

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
    LArray <$> simplifyLExp name <*> simplifyAExp idx
simplifyLExp' (LStructurePart struct part) =
    LStructurePart <$> simplifyLExp struct <*> return part
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
    Update <$> simplifyState state <*> simplifyLExp lExp <*> simplifyAExp aExp