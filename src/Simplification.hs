module Simplification where
import AST
import Control.Monad.Reader
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

type Inequality = Set LSExp

type Simplified = ReaderT (Set Inequality) Updated

notEqual :: LSExp -> LSExp -> Inequality
notEqual l r = Set.fromList [l, r]

findInequalities :: FOSExp -> Set Inequality
findInequalities (FOComp NotEqual (ASRead (ReadLExp s1 l)) (ASRead (ReadLExp s2 r))) = 
    if s1 == s2 
        then Set.singleton $ notEqual l r
        else Set.empty
findInequalities (FOBinExp And fo1 fo2) = Set.union (findInequalities fo1) (findInequalities fo2)
findInequalities (FOBinExp Implies fo _) = findInequalities fo
findInequalities _ = Set.empty

simplify :: FOSExp -> FOSExp
simplify a =
    let inequalities = traceShowId $ findInequalities a
    in  case runReaderT (simplifyFOSExp a) inequalities of
            Updated updated -> simplify updated
            Unchanged unchanged -> unchanged

simplifyFOSExp :: FOSExp -> Simplified FOSExp
simplifyFOSExp FOTrue = return FOTrue
simplifyFOSExp FOFalse = return FOFalse
simplifyFOSExp (FOComp op l r) = do
    updatedL <- simplifyASExp l
    updatedR <- simplifyASExp r
    return $ FOComp op updatedL updatedR
simplifyFOSExp (FONeg fo) = FONeg <$> simplifyFOSExp fo
simplifyFOSExp (FOBinExp op l r) = do
    updatedL <- simplifyFOSExp l
    updatedR <- simplifyFOSExp r
    return $ FOBinExp op updatedL updatedR
simplifyFOSExp (Forall i fo) = Forall i <$> simplifyFOSExp fo
simplifyFOSExp (Exists i fo) = Exists i <$> simplifyFOSExp fo
simplifyFOSExp (Predicate i fos) = Predicate i <$> mapM simplifyASExp fos 

simplifyASExp :: ASExp -> Simplified ASExp
simplifyASExp (ASLit a) = return $ ASLit a
simplifyASExp (ASLogVar v) = return $ ASLogVar v
simplifyASExp (ASRead r) = simplifyASRead r
simplifyASExp (ASBinExp op l r) = do
    updatedL <- simplifyASExp l
    updatedR <- simplifyASExp r
    return $ ASBinExp op updatedL updatedR
simplifyASExp (ASArray fields) = ASArray <$> mapM simplifyASExp fields
simplifyASExp (ASFunCall funName funArgs) = ASFunCall funName <$> mapM simplifyASExp funArgs

simplifyLSExp :: LSExp -> Simplified LSExp
simplifyLSExp (LSIdt i) = return $ LSIdt i
simplifyLSExp (LSArray name idx) = do
    newName <- simplifyLSExp name
    newIdx <- simplifyASExp idx
    return $ LSArray newName newIdx
simplifyLSExp (LSStructPart struct part) = do
    newStruct <- simplifyLSExp struct
    return $ LSStructPart newStruct part
simplifyLSExp (LSRead r) = simplifyLSRead r

simplifyReadLExp :: ReadLExp -> Simplified ReadLExp
simplifyReadLExp (ReadLExp state loc) = do
    simplifiedState <- simplifyState state
    simplifiedLoc <- simplifyLSExp loc
    return $ ReadLExp simplifiedState simplifiedLoc

simplifyLSRead :: ReadLExp -> Simplified LSExp
simplifyLSRead l = simplifyReadLExp l >>= simplifyLSRead'
simplifyLSRead' :: ReadLExp -> Simplified LSExp
simplifyLSRead' original@(ReadLExp (Update state lSExp _) toRead) = do
    memComparison <- compareLSExp toRead lSExp
    LSRead <$> case memComparison of
        MemEq -> error "do something here"
        MemNotEq -> update $ ReadLExp state toRead 
        MemUndecidable -> return original
simplifyLSRead' original = return $ LSRead original

simplifyASRead :: ReadLExp -> Simplified ASExp
simplifyASRead l = simplifyReadLExp l >>= simplifyASRead'
simplifyASRead' :: ReadLExp -> Simplified ASExp
simplifyASRead' original@(ReadLExp (Update state lSExp aSExp) toRead) = do
    memComparison <- compareLSExp toRead lSExp
    case memComparison of
        MemEq -> update aSExp
        MemNotEq -> update $ ASRead $ ReadLExp state toRead 
        MemUndecidable -> return $ ASRead original
simplifyASRead' original = return $ ASRead original

simplifyState :: State -> Simplified State
simplifyState (Update state lSExp aSExp) = do
    simplifiedState <- simplifyState state
    simplifiedLSExp <- simplifyLSExp lSExp
    simplifiedASExp <- simplifyASExp aSExp
    simplifyState' (Update simplifiedState simplifiedLSExp simplifiedASExp)
simplifyState atomic = return atomic    
simplifyState' :: State -> Simplified State
simplifyState'  original@(Update (Update s l1 _) l2 w) = do
    memComparison <- compareLSExp l1 l2
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


compareLSExp :: LSExp -> LSExp -> Simplified MemEq
compareLSExp a b = if (a == b) 
    then return MemEq
    else do
        inequalities <- ask
        let predefindedNEq = Set.member (notEqual a b) inequalities
        let noRefA = isNotRead a
        let noRefB = isNotRead b
        return $ if predefindedNEq || (noRefA && noRefB) then MemNotEq else MemUndecidable

isNotRead :: LSExp -> Bool
isNotRead (LSRead _) = False
isNotRead _ = True

update :: a -> Simplified a
update a = lift $ Updated a