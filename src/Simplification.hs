{-# LANGUAGE GADTs #-}
module Simplification where
import AST
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Inequality = Set LExpFO

type Simplified = ReaderT SimplificationCtx Updated

type BExpFO = BExp FO Refs
type AExpFO = AExp FO Refs
type LExpFO = LExp FO Refs
type ReadLExpFO = ReadLExp FO

data SimplificationCtx = SimplificationCtx 
    { inequalities :: Set Inequality
    , localVars :: Set LExpFO
    }

notEqual :: LExpFO -> LExpFO -> Inequality
notEqual l r = Set.fromList [l, r]

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
            { inequalities = traceShowId $ findInequalities a
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
        MemEq -> error "do something here"
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

data Updated a = Updated a | Unchanged a deriving (Eq, Show)

unwrap :: Updated a -> a
unwrap (Updated a) = a
unwrap (Unchanged a) = a

instance Functor Updated where
    fmap f a = pure f <*> a

instance Applicative Updated where
    pure a = Unchanged a
    (Updated f) <*> a = Updated $ f $ unwrap a
    f <*> (Updated a) = Updated $ unwrap f a
    (Unchanged f) <*> (Unchanged a) = Unchanged $ f a

instance Monad Updated where
    return a = Unchanged a
    (Updated a) >>= f = Updated $ unwrap $ f a
    (Unchanged a) >>= f = f a

data MemEq = MemEq | MemNotEq |MemUndecidable


compareDifferentLExp :: LExpFO -> LExpFO -> Simplified MemEq
compareDifferentLExp (LRead _) _ = return MemUndecidable
compareDifferentLExp _ (LRead _) = return MemUndecidable
compareDifferentLExp (LStructurePart struct1 idt1) (LStructurePart struct2 idt2) =
    if idt1 == idt2 then compareLExp struct1 struct2 else return MemNotEq
compareDifferentLExp (LArray arr1 idx1) (LArray arr2 idx2) = do
    eqArray <- compareLExp arr1 arr2
    eqIdx <- compareAExp idx1 idx2
    return $ eqArray `memAnd` eqIdx
compareDifferentLExp _ _ = return MemNotEq

-- data ReadLExp l = ReadLExp State (LExp l Refs)


compareAExp :: AExpFO -> AExpFO -> Simplified MemEq
compareAExp a b | a == b = return MemEq
compareAExp (ALit _) (ALit _) = return MemNotEq
compareAExp (ARead (ReadLExp s1 l1)) (ARead (ReadLExp s2 l2)) | s1 == s2 = do
    ineqs <- inequalities <$> ask
    let predefindedNEq = Set.member (notEqual l1 l2) ineqs
    sameLExp <- compareLExp l1 l2
    return $ case sameLExp of
        MemEq -> MemEq
        _ -> if predefindedNEq then MemNotEq else MemUndecidable
compareAExp _ _ = return MemUndecidable

memAnd :: MemEq -> MemEq -> MemEq
memAnd MemUndecidable _ = MemUndecidable
memAnd _ MemUndecidable = MemUndecidable
memAnd MemEq MemEq = MemEq
memAnd _ _ = MemNotEq


compareLExp :: LExpFO -> LExpFO -> Simplified MemEq
compareLExp a b | (a == b) = return MemEq
compareLExp a b = do
    ineqs <- inequalities <$> ask
    decls <- localVars <$> ask
    let predefindedNEq = Set.member (notEqual a b) ineqs
    let anyIsFresh = Set.member a decls || Set.member b decls
    if predefindedNEq || anyIsFresh 
        then return MemNotEq 
        else compareDifferentLExp a b

isNotRead :: LExpFO -> Bool
isNotRead (LRead _) = False
isNotRead _ = True

update :: a -> Simplified a
update a = lift $ Updated a