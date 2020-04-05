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
-- findInequalities (BComp NotEqual (AIdt l1) (AIdt l2)) = 
--     if s1 == s2 
--         then Set.singleton $ notEqual l1 l2
--         else Set.empty
findInequalities (BComp NotEqual (AIdt l1) (AIdt l2)) = Set.singleton $ notEqual l1 l2
findInequalities (BBinExp And fo1 fo2) = Set.union (findInequalities fo1) (findInequalities fo2)
findInequalities (BBinExp Implies fo _) = findInequalities fo
findInequalities _ = Set.empty

simplify :: BExpFO -> BExpFO
simplify = simplifyLocalVars Set.empty

simplifyLocalVars :: Set LExpFO -> BExpFO -> BExpFO
simplifyLocalVars locals a =
    let ctx = SimplificationCtx 
            { inequalities = findInequalities a
            , localVars = locals
            }
    in  case runReaderT (simplifyBExpFO a) ctx of
            Updated updated -> simplifyLocalVars locals updated
            Unchanged unchanged -> unchanged

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
    updatedR <- simplifyBExpFO r
    return $ BBinExp op updatedL updatedR
simplifyBExpFO (BForall i fo) = BForall i <$> simplifyBExpFO fo
simplifyBExpFO (BExists i fo) = BExists i <$> simplifyBExpFO fo
simplifyBExpFO (BPredicate i fos) = BPredicate i <$> mapM simplifyAExpFO fos 

simplifyAExpFO :: AExpFO -> Simplified AExpFO
simplifyAExpFO (ALit a) = return $ ALit a
simplifyAExpFO (ALogVar v) = return $ ALogVar v
simplifyAExpFO (ABinExp op l r) = do
    updatedL <- simplifyAExpFO l
    updatedR <- simplifyAExpFO r
    return $ ABinExp op updatedL updatedR
simplifyAExpFO (AFunCall funName funArgs) = AFunCall funName <$> mapM simplifyAExpFO funArgs
simplifyAExpFO (AIdt l) = do
    simplifiedLExp <- simplifyLExp l
    case simplifiedLExp of 
        LRead (Update state lExp aExp) toRead -> do
            memComparison <- compareLExp toRead lExp
            case memComparison of
                MemEq -> update aExp
                MemNotEq -> update $ AIdt $ LRead state toRead 
                MemUndecidable -> return $ AIdt simplifiedLExp
        _ -> return $ AIdt simplifiedLExp

simplifyLExp :: LExpFO -> Simplified LExpFO
simplifyLExp (LIdt i) = return $ LIdt i
simplifyLExp (LArray name idx) = do
    newName <- simplifyLExp name
    newIdx <- simplifyAExpFO idx
    return $ LArray newName newIdx
simplifyLExp (LStructurePart struct part) = do
    newStruct <- simplifyLExp struct
    return $ LStructurePart newStruct part
simplifyLExp (LRead state' toRead') = do
    state <- simplifyState state'
    toRead <- simplifyLExp toRead'
    case state of 
        (Update nestedState toUpdate _) -> do
            memComparison <- compareLExp toRead toUpdate
            case memComparison of
                MemNotEq -> update $ LRead nestedState toRead 
                _ -> return $ LRead state toRead
        _ -> return $ LRead state toRead

simplifyState :: State -> Simplified State
simplifyState (Update state lExp aExp) = do
    simplifiedState <- simplifyState state
    simplifiedLExp <- simplifyLExp lExp
    simplifiedAExp <- simplifyAExpFO aExp
    simplifyState' (Update simplifiedState simplifiedLExp simplifiedAExp)
simplifyState atomic = return atomic    
simplifyState' :: State -> Simplified State
simplifyState'  original@(Update (Update s l1 _) l2 w) = do
    memComparison <- compareLExp l1 l2
    case memComparison of
        MemEq -> update $ Update s l2 w
        _ -> return original
simplifyState' original = return original