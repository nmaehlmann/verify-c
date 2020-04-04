{-# LANGUAGE GADTs #-}
module SMT.Export (export) where
import AST
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

data SMTDecl = SMTConst String | SMTUnary String deriving (Eq, Ord)

export :: String -> BExp FO Plain -> Maybe String
export env b = fmap export' $ bAssert b
    where 
        export' assertion = unlines $ [arrayAccessDecl, derefDecl] ++ decls ++ [env, assertion, checkSat]
        decls = map mkDecl $ Set.toList $ bDecls b

bAssert :: BExp FO Plain -> Maybe String
bAssert b = do
    smtB <- bToSMT (BNeg b)
    return $ sExp ["assert", smtB]

bToSMT :: BExp FO Plain -> Maybe String
bToSMT BTrue = return "true"
bToSMT BFalse = return "false"
bToSMT (BComp NotEqual l r) = bToSMT $ BNeg $ BComp Equal l r
bToSMT (BComp op l r) = do
    smtL <- aToSMT l
    smtR <- aToSMT r
    return $ sExp [show op, smtL, smtR]
bToSMT (BNeg b) = do
    smtB <- bToSMT b
    return $ sExp ["not", smtB]
bToSMT (BBinExp op l r) = do
    smtL <- bToSMT l
    smtR <- bToSMT r
    return $ sExp [binOpToSMT op, smtL, smtR]
bToSMT (BForall i b) = do
    smtB <- bToSMT b
    return $ sExp ["forall", "((" ++ show i ++ " Int" ++ "))", smtB]
bToSMT (BExists i b) = do
    smtB <- bToSMT b
    return $ sExp ["exists", "((" ++ show i ++ " Int" ++ "))", smtB]
bToSMT (BPredicate (Idt name) args) = do
    smtArgs <- mapM aToSMT args
    return $ sExp $ name : smtArgs

binOpToSMT :: BBinOp -> String
binOpToSMT And = "and"
binOpToSMT Or = "or"
binOpToSMT Implies = "implies"
binOpToSMT Iff = "="

aToSMT :: AExp FO Plain -> Maybe String
aToSMT (ALit i) = return $ show i
aToSMT (AIdt l) = lToSMT l
aToSMT (ABinExp op l r) = do
    smtL <- aToSMT l
    smtR <- aToSMT r
    return $ sExp [show op, smtL, smtR]
aToSMT (ALogVar v) = return $ show v
aToSMT (AFunCall (Idt name) args) = do
    smtArgs <- mapM aToSMT args
    return $ sExp $ name : smtArgs
aToSMT (AAddress _) = Nothing

lToSMT :: LExp FO Plain -> Maybe String
lToSMT (LIdt i) = return $ show i
lToSMT (LArray lExp aExp) = do
    smtLExp <- lToSMT lExp
    smtAExp <- aToSMT aExp
    return $ sExp [readArray, smtLExp, smtAExp]
lToSMT (LStructurePart lExp (Idt accessor)) = do
    smtLExp <- lToSMT lExp
    return $ sExp [accessor, smtLExp]
lToSMT (LDeref lExp) = do
    smtLExp <- lToSMT lExp
    return $ sExp [deref, smtLExp]

sExp :: [String] -> String
sExp s = "(" ++ concat (intersperse " " s) ++ ")"

bDecls :: BExp FO Plain -> Set SMTDecl
bDecls BTrue = Set.empty
bDecls BFalse = Set.empty
bDecls (BComp _ l r) = Set.union (aDecls l) (aDecls r)
bDecls (BNeg b) = bDecls b
bDecls (BBinExp _ l r) = Set.union (bDecls l) (bDecls r)
bDecls (BForall (Idt s) b) = Set.delete (SMTConst s) $ bDecls b
bDecls (BExists (Idt s) b) = Set.delete (SMTConst s) $ bDecls b
bDecls (BPredicate _ args) = foldl Set.union Set.empty $ map aDecls args

aDecls :: AExp FO Plain -> Set SMTDecl
aDecls (ALit _) = Set.empty
aDecls (AIdt l) = lDecls l
aDecls (ABinExp _ l r) = Set.union (aDecls l) (aDecls r)
aDecls (ALogVar (Idt v)) = Set.singleton $ SMTConst v
aDecls (AFunCall _ args) = foldl Set.union Set.empty $ map aDecls args
aDecls (AAddress l) = lDecls l

lDecls :: LExp FO Plain -> Set SMTDecl
lDecls (LIdt (Idt s)) = Set.singleton $ SMTConst s
lDecls (LArray lExp aExp) = Set.union (lDecls lExp) (aDecls aExp)
lDecls (LStructurePart lExp _) = (lDecls lExp)
lDecls (LDeref i) = lDecls i

readArray :: String
readArray = "read_array"

arrayAccessDecl :: String
arrayAccessDecl = sExp ["declare-fun", readArray, "(Int Int)", "Int"]

deref :: String
deref = "deref"

derefDecl :: String
derefDecl = mkDecl $ SMTUnary deref

mkDecl :: SMTDecl -> String
mkDecl (SMTConst s) = sExp ["declare-fun", s, "()", "Int"]
mkDecl (SMTUnary s) = sExp ["declare-fun", s, "(Int)", "Int"]

checkSat :: String
checkSat = sExp ["check-sat"]