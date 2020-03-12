module VC where

import AST

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader

verify :: FunctionDefinition -> [FOSExp]
verify f = map simplify $ FOBinExp Implies precondition awpBody : wvcs
    where
        awpBody = awp (funDefBody f) postcondition postcondition
        precondition = stateifyFO $ funDefPrecond f
        postcondition = stateifyFO $ funDefPostcond f
        wvcs = wvc (funDefBody f) FOFalse postcondition

type VC = Reader Ctx

data Ctx = Ctx 
    { preconditions :: Map Idt FOSExp
    , postconditions :: Map Idt FOSExp
    }

generateContext :: Program -> Ctx
generateContext (Program fs) = Ctx {preconditions = pres, postconditions = posts}
    where (pres, posts) = generateContext' fs

generateContext' :: [FunctionDefinition] -> (Map Idt FOSExp, Map Idt FOSExp)
generateContext' [] = (Map.empty, Map.empty)
generateContext' (f:fs) = 
    let insert = Map.insert (funDefName f)
        pre = stateifyFO $ funDefPrecond f
        post = stateifyFO $ funDefPostcond f
        (prevPre, prevPost) = generateContext' fs
    in  (insert pre prevPre, insert post prevPost)

awp :: Stmt -> FOSExp -> FOSExp -> FOSExp
awp Empty q _ = q
awp (Assertion q) _ _ = stateifyFO q
awp (ITE condition sTrue sFalse) q qr = 
    let awpTrue = awp sTrue q qr
        awpFalse = awp sFalse q qr
        foCondition = stateifyFO $ bExpToFOExp condition
        foTrue  = FOBinExp And foCondition awpTrue
        foFalse = FOBinExp And (FONeg foCondition) awpFalse
    in FOBinExp Or foTrue foFalse
awp (While _ inv _) _ _ = stateifyFO inv
awp (Seq s1 s2) q qr = awp s1 (awp s2 q qr) qr
awp (Assignment idt aExp) q _ = 
    let newState = Update sigma (dagger idt) (hashmark aExp)
        oldState = sigma
    in  replaceState oldState newState q
awp (Return Nothing) _ qr = qr
awp (Return _) _ _ = error "unsupported yet"

wvc :: Stmt -> FOSExp -> FOSExp -> [FOSExp]
wvc Empty _ _ = []
wvc (Assignment _ _ ) _ _ = []
wvc (Seq s1 s2) q qr = wvc s1 (awp s2 q qr) qr ++ wvc s2 q qr
wvc (ITE _ sTrue sFalse) q qr = wvc sTrue q qr ++ wvc sFalse q qr
wvc (While cond inv body) q qr =
    let foCond = stateifyFO $ bExpToFOExp cond
        foInv = stateifyFO inv
        foInvAndCond = FOBinExp And foInv foCond
        foInvAndNotCond = FOBinExp And foInv $ FONeg foCond
    in  FOBinExp Implies foInvAndCond (awp body foInv qr)
        : FOBinExp Implies foInvAndNotCond q
        : wvc body foInv qr
wvc (Assertion fo) q _ = [FOBinExp Implies (stateifyFO fo) q]
wvc (Return _) _ _ = []

replaceState :: State -> State -> FOSExp -> FOSExp
replaceState sOld sNew expOld = fmap (replaceStateInASExp sOld sNew) expOld

replaceStateInASExp :: State -> State -> ASExp -> ASExp
replaceStateInASExp sOld sNew aSExp = replaceStateInASExp' sOld sNew aSExp

replaceStateInASExp' :: State -> State -> ASExp -> ASExp
replaceStateInASExp' _ _ (ASLit i) = ASLit i
replaceStateInASExp' sOld sNew (ASRead readLExp) = ASRead $ replaceStateInRead sOld sNew readLExp
replaceStateInASExp' sOld sNew (ASBinExp op l r) = ASBinExp op (replaceStateInASExp sOld sNew l) (replaceStateInASExp sOld sNew r)
replaceStateInASExp' sOld sNew (ASArray fields) = ASArray $ map (replaceStateInASExp sOld sNew) fields
replaceStateInASExp' sOld sNew (ASFunCall name args) = ASFunCall name $ map (replaceStateInASExp sOld sNew) args

replaceStateInRead :: State -> State -> ReadLExp -> ReadLExp
replaceStateInRead sOld sNew (ReadLExp sNested lSExp) = ReadLExp (replaceStateInNestedState sOld sNew sNested) (replaceStateInLSExp sOld sNew lSExp)

replaceStateInNestedState :: State -> State -> State -> State
replaceStateInNestedState sOld sNew sNested | sOld == sNested = sNew
replaceStateInNestedState sOld sNew (Update state lSExp aSExp) = Update 
    (replaceStateInNestedState sOld sNew state) 
    (replaceStateInLSExp sOld sNew lSExp)
    (replaceStateInASExp sOld sNew aSExp)
replaceStateInNestedState _ _ s = s

replaceStateInLSExp :: State -> State -> LSExp -> LSExp
replaceStateInLSExp _ _ (LSIdt idt) = LSIdt idt
replaceStateInLSExp sOld sNew (LSArray lSExp aSExp) = LSArray (replaceStateInLSExp sOld sNew lSExp) (replaceStateInASExp sOld sNew aSExp)
replaceStateInLSExp sOld sNew (LSStructPart lSExp idt) = LSStructPart (replaceStateInLSExp sOld sNew lSExp) idt
replaceStateInLSExp sOld sNew (LSRead readLExp) = LSRead $ replaceStateInRead sOld sNew readLExp

bExpToFOExp :: BExp -> FOExp
bExpToFOExp BTrue = FOTrue
bExpToFOExp BFalse = FOFalse
bExpToFOExp (BComp op l r) = FOComp op l r
bExpToFOExp (BNeg b) = FONeg $ bExpToFOExp b
bExpToFOExp (BBinExp op l r) = FOBinExp op (bExpToFOExp l) (bExpToFOExp r)

stateifyFO :: FOExp -> FOSExp
stateifyFO = fmap stateifyAExp

stateifyAExp :: AExp -> ASExp
stateifyAExp = hashmark

dagger :: LExp -> LSExp
dagger (LIdt idt) = LSIdt idt
dagger (LArray idt idx) = LSArray (dagger idt) (hashmark idx)
dagger (LStructPart struct part) = LSStructPart (dagger struct) part
dagger (LDereference idt) = LSRead $ ReadLExp sigma $ dagger idt

hashmark :: AExp -> ASExp
hashmark (ALit i) = ASLit i
hashmark (AIdt l) = ASRead $ ReadLExp sigma $ dagger l
hashmark (ABinExp op l r) = ASBinExp op (hashmark l) (hashmark r)
hashmark (AArray fields) = ASArray $ map hashmark fields
hashmark (AFunCall name args) = ASFunCall name $ map hashmark args

sigma :: State
sigma = Atomic "s"

simplify :: FOSExp -> FOSExp
simplify a = case simplifyFOSExp a of
    Updated updated -> simplify updated
    Unchanged unchanged -> unchanged

simplifyFOSExp :: FOSExp -> Updated FOSExp
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

simplifyASExp :: ASExp -> Updated ASExp
simplifyASExp (ASLit a) = Unchanged $ ASLit a
simplifyASExp (ASRead (ReadLExp s l)) = do
    newS <- simplifyState2 s
    newL <- simplifyLSExp l
    simplifyRead $ ReadLExp newS newL

simplifyASExp (ASBinExp op l r) = do
    updatedL <- simplifyASExp l
    updatedR <- simplifyASExp r
    return $ ASBinExp op updatedL updatedR
simplifyASExp (ASArray fields) = ASArray <$> mapM simplifyASExp fields
simplifyASExp (ASFunCall funName funArgs) = ASFunCall funName <$> mapM simplifyASExp funArgs

simplifyLSExp :: LSExp -> Updated LSExp
simplifyLSExp (LSIdt i) = return $ LSIdt i
simplifyLSExp (LSArray name idx) = do
    newName <- simplifyLSExp name
    newIdx <- simplifyASExp idx
    return $ LSArray newName newIdx
simplifyLSExp (LSStructPart struct part) = do
    newStruct <- simplifyLSExp struct
    return $ LSStructPart newStruct part
simplifyLSExp (LSRead r) = LSRead <$> simplifyLSRead r

simplifyLSRead :: ReadLExp -> Updated ReadLExp
simplifyLSRead original@(ReadLExp (Update state lSExp _) toRead) = case compareLSExp toRead lSExp of
    MemNotEq -> Updated $ ReadLExp state toRead 
    _ -> Unchanged original
simplifyLSRead original = Unchanged original

simplifyRead :: ReadLExp -> Updated ASExp
simplifyRead original@(ReadLExp (Update state lSExp aSExp) toRead) = case compareLSExp toRead lSExp of
    MemEq -> Updated aSExp
    MemNotEq -> Updated $ ASRead $ ReadLExp state toRead 
    MemUndecidable -> Unchanged $ ASRead original
simplifyRead original = Unchanged $ ASRead original

simplifyState2 :: State -> Updated State
simplifyState2  original@(Update (Update s l1 _) l2 w) = case compareLSExp l1 l2 of
    MemEq -> Updated $ Update s l2 w
    _ -> Unchanged original
simplifyState2 original = Unchanged original

simplifyState :: ReadLExp -> Updated ReadLExp
simplifyState (ReadLExp s toRead) = do
    updatedS <- simplifyState2 s
    return $ ReadLExp updatedS toRead

data Updated a = Updated a | Unchanged a deriving Show

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

compareLSExp :: LSExp -> LSExp -> MemEq
-- compareLSExp (LSRead _) _ = MemUndecidable
-- compareLSExp _ (LSRead _) = MemUndecidable
compareLSExp a b = if a == b then MemEq else MemNotEq