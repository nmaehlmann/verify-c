{-# LANGUAGE GADTs #-}
module Simplification where
import AST
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Inequality = Set LExprFO

type Simplified = ReaderT SimplificationCtx Updated

type BExprFO = BExpr FO Refs
type AExprFO = AExpr FO Refs
type LExprFO = LExpr FO Refs
type ReadLExprFO = ReadLExpr FO

data SimplificationCtx = SimplificationCtx 
    { inequalities :: Set Inequality
    , localVars :: Set LExprFO
    }

notEqual :: LExprFO -> LExprFO -> Inequality
notEqual l r = Set.fromList [l, r]

findInequalities :: BExprFO -> Set Inequality
findInequalities (BComp NotEqual (ARead (ReadLExpr s1 l)) (ARead (ReadLExpr s2 r))) = 
    if s1 == s2 
        then Set.singleton $ notEqual l r
        else Set.empty
findInequalities (BBinExp And fo1 fo2) = Set.union (findInequalities fo1) (findInequalities fo2)
findInequalities (BBinExp Implies fo _) = findInequalities fo
findInequalities _ = Set.empty

simplify :: BExprFO -> BExprFO
simplify = simplifyLocalVars Set.empty

simplifyLocalVars :: Set LExprFO -> BExprFO -> BExprFO
simplifyLocalVars locals a =
    let ctx = SimplificationCtx 
            { inequalities = traceShowId $ findInequalities a
            , localVars = locals
            }
    in  case runReaderT (simplifyBExprFO a) ctx of
            Updated updated -> simplifyLocalVars locals updated
            Unchanged unchanged -> unchanged

simplifyBExprFO :: BExprFO -> Simplified BExprFO
simplifyBExprFO BTrue = return BTrue
simplifyBExprFO BFalse = return BFalse
simplifyBExprFO (BComp op l r) = do
    updatedL <- simplifyAExprFO l
    updatedR <- simplifyAExprFO r
    return $ BComp op updatedL updatedR
simplifyBExprFO (BNeg fo) = BNeg <$> simplifyBExprFO fo
simplifyBExprFO (BBinExp op l r) = do
    updatedL <- simplifyBExprFO l
    updatedR <- simplifyBExprFO r
    return $ BBinExp op updatedL updatedR
simplifyBExprFO (BForall i fo) = BForall i <$> simplifyBExprFO fo
simplifyBExprFO (BExists i fo) = BExists i <$> simplifyBExprFO fo
simplifyBExprFO (BPredicate i fos) = BPredicate i <$> mapM simplifyAExprFO fos 

simplifyAExprFO :: AExprFO -> Simplified AExprFO
simplifyAExprFO (ALit a) = return $ ALit a
simplifyAExprFO (ALogVar v) = return $ ALogVar v
simplifyAExprFO (ARead r) = simplifyARead r
simplifyAExprFO (ABinExp op l r) = do
    updatedL <- simplifyAExprFO l
    updatedR <- simplifyAExprFO r
    return $ ABinExp op updatedL updatedR
simplifyAExprFO (AArray fields) = AArray <$> mapM simplifyAExprFO fields
simplifyAExprFO (AFunCall funName funArgs) = AFunCall funName <$> mapM simplifyAExprFO funArgs

simplifyLExpr :: LExprFO -> Simplified LExprFO
simplifyLExpr (LIdt i) = return $ LIdt i
simplifyLExpr (LArray name idx) = do
    newName <- simplifyLExpr name
    newIdx <- simplifyAExprFO idx
    return $ LArray newName newIdx
simplifyLExpr (LStructurePart struct part) = do
    newStruct <- simplifyLExpr struct
    return $ LStructurePart newStruct part
simplifyLExpr (LRead r) = simplifyLRead r

simplifyReadLExpr :: ReadLExprFO -> Simplified ReadLExprFO
simplifyReadLExpr (ReadLExpr state loc) = do
    simplifiedState <- simplifyState state
    simplifiedLoc <- simplifyLExpr loc
    return $ ReadLExpr simplifiedState simplifiedLoc

simplifyLRead :: ReadLExprFO -> Simplified LExprFO
simplifyLRead l = simplifyReadLExpr l >>= simplifyLRead'
simplifyLRead' :: ReadLExprFO -> Simplified LExprFO
simplifyLRead' original@(ReadLExpr (Update state lExpr _) toRead) = do
    memComparison <- compareLExpr toRead lExpr
    LRead <$> case memComparison of
        MemEq -> error "do something here"
        MemNotEq -> update $ ReadLExpr state toRead 
        MemUndecidable -> return original
simplifyLRead' original = return $ LRead original

simplifyARead :: ReadLExprFO -> Simplified AExprFO
simplifyARead l = simplifyReadLExpr l >>= simplifyARead'
simplifyARead' :: ReadLExprFO -> Simplified AExprFO
simplifyARead' original@(ReadLExpr (Update state lExpr aExpr) toRead) = do
    memComparison <- compareLExpr toRead lExpr
    case memComparison of
        MemEq -> update aExpr
        MemNotEq -> update $ ARead $ ReadLExpr state toRead 
        MemUndecidable -> return $ ARead original
simplifyARead' original = return $ ARead original

simplifyState :: State -> Simplified State
simplifyState (Update state lExpr aSExp) = do
    simplifiedState <- simplifyState state
    simplifiedLExpr <- simplifyLExpr lExpr
    simplifiedAExpr <- simplifyAExprFO aSExp
    simplifyState' (Update simplifiedState simplifiedLExpr simplifiedAExpr)
simplifyState atomic = return atomic    
simplifyState' :: State -> Simplified State
simplifyState'  original@(Update (Update s l1 _) l2 w) = do
    memComparison <- compareLExpr l1 l2
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


compareLExpr :: LExprFO -> LExprFO -> Simplified MemEq
compareLExpr a b = if (a == b) 
    then return MemEq
    else do
        ineqs <- inequalities <$> ask
        decls <- localVars <$> ask
        let predefindedNEq = Set.member (notEqual a b) ineqs
        let bothAreNotRef = isNotRead a && isNotRead b
        let anyIsFresh = Set.member a decls || Set.member b decls
        return $ if predefindedNEq || bothAreNotRef || anyIsFresh then MemNotEq else MemUndecidable

isNotRead :: LExprFO -> Bool
isNotRead (LRead _) = False
isNotRead _ = True

update :: a -> Simplified a
update a = lift $ Updated a