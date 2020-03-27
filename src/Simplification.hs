{-# LANGUAGE GADTs #-}
module Simplification where
import AST
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import Simplified
import FOTypes
import MemEq

findInequalities :: BExpFO -> Set Inequality
findInequalities (BComp NotEqual (ARead (ReadLExp s1 l)) (ARead (ReadLExp s2 r))) = 
    if s1 == s2 
        then Set.singleton $ notEqual l r
        else Set.empty
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
simplifyAExpFO (ARead r) = simplifyARead r
simplifyAExpFO (ABinExp op l r) = do
    updatedL <- simplifyAExpFO l
    updatedR <- simplifyAExpFO r
    return $ ABinExp op updatedL updatedR
simplifyAExpFO (AArray fields) = AArray <$> mapM simplifyAExpFO fields
simplifyAExpFO (AFunCall funName funArgs) = AFunCall funName <$> mapM simplifyAExpFO funArgs

simplifyLExp :: LExpFO -> Simplified LExpFO
simplifyLExp (LIdt i) = return $ LIdt i
simplifyLExp (LArray name idx) = do
    newName <- simplifyLExp name
    newIdx <- simplifyAExpFO idx
    return $ LArray newName newIdx
simplifyLExp (LStructurePart struct part) = do
    newStruct <- simplifyLExp struct
    return $ LStructurePart newStruct part
simplifyLExp (LRead r) = simplifyLRead r

simplifyReadLExp :: ReadLExpFO -> Simplified ReadLExpFO
simplifyReadLExp (ReadLExp state loc) = do
    simplifiedState <- simplifyState state
    simplifiedLoc <- simplifyLExp loc
    return $ ReadLExp simplifiedState simplifiedLoc

simplifyLRead :: ReadLExpFO -> Simplified LExpFO
simplifyLRead l = simplifyReadLExp l >>= simplifyLRead'
simplifyLRead' :: ReadLExpFO -> Simplified LExpFO
simplifyLRead' original@(ReadLExp (Update state lExp _) toRead) = do
    memComparison <- compareLExp toRead lExp
    LRead <$> case memComparison of
        MemEq -> error "TODO: handle equal LExp in read"
        MemNotEq -> update $ ReadLExp state toRead 
        MemUndecidable -> return original
simplifyLRead' original = return $ LRead original

simplifyARead :: ReadLExpFO -> Simplified AExpFO
simplifyARead l = simplifyReadLExp l >>= simplifyARead'
simplifyARead' :: ReadLExpFO -> Simplified AExpFO
simplifyARead' original@(ReadLExp (Update state lExp aExp) toRead) = do
    memComparison <- compareLExp toRead lExp
    case memComparison of
        MemEq -> update aExp
        MemNotEq -> update $ ARead $ ReadLExp state toRead 
        MemUndecidable -> return $ ARead original
simplifyARead' original = return $ ARead original

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