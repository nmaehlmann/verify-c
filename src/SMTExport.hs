{-# LANGUAGE GADTs #-}
module SMTExport where
import AST
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

toSMT :: BExp FO Plain -> String
toSMT b =
    let decls = map mkDecl $ Set.toList $ bDecls b
        assertion = bAssert b
    in  unlines $ [arrayAccessDecl, derefDecl] ++ decls ++ [assertion, checkSat]

bAssert :: BExp FO Plain -> String
bAssert b = sExp ["assert", bToSMT (BNeg b)]

bToSMT :: BExp FO Plain -> String
bToSMT BTrue = "true"
bToSMT BFalse = "false"
bToSMT (BComp NotEqual l r) = bToSMT $ BNeg $ BComp Equal l r
bToSMT (BComp op l r) = sExp [show op, aToSMT l, aToSMT r]
bToSMT (BNeg b) = sExp ["not", bToSMT b]
bToSMT (BBinExp op l r) = sExp [binOpToSMT op, bToSMT l, bToSMT r]
bToSMT (BForall i b) = sExp ["forall", "((" ++ show i ++ " Int" ++ "))", bToSMT b]
bToSMT (BExists i b) = sExp ["exists", "((" ++ show i ++ " Int" ++ "))", bToSMT b]
bToSMT (BPredicate (Idt name) args) = sExp $ name : map aToSMT args

binOpToSMT :: BBinOp -> String
binOpToSMT And = "and"
binOpToSMT Or = "or"
binOpToSMT Implies = "implies"
binOpToSMT Iff = "="

aToSMT :: AExp FO Plain -> String
aToSMT (ALit i) = show i
aToSMT (AIdt l) = lToSMT l
aToSMT (ABinExp op l r) = sExp [show op, aToSMT l, aToSMT r]
aToSMT (ALogVar v) = show v
aToSMT (AFunCall (Idt name) args) = sExp $ name : map aToSMT args
aToSMT (AArray _) = error "unsupported array"

lToSMT :: LExp FO Plain -> String
lToSMT (LIdt i) = show i
lToSMT (LArray lExp aExp) = sExp [readArray, lToSMT lExp, aToSMT aExp]
lToSMT (LStructurePart lExp (Idt accessor)) = sExp [accessor, lToSMT lExp]
lToSMT (LDeref i) = sExp [deref, lToSMT i]

sExp :: [String] -> String
sExp s = "(" ++ concat (intersperse " " s) ++ ")"

data SMTDecl = SMTConst String | SMTUnary String deriving (Eq, Ord)

bDecls :: BExp FO Plain -> Set SMTDecl
bDecls BTrue = Set.empty
bDecls BFalse = Set.empty
bDecls (BComp _ l r) = Set.union (aDecls l) (aDecls r)
bDecls (BNeg b) = bDecls b
bDecls (BBinExp _ l r) = Set.union (bDecls l) (bDecls r)
bDecls (BForall _ b) = bDecls b
bDecls (BExists _ b) = bDecls b
bDecls (BPredicate _ args) = foldl Set.union Set.empty $ map aDecls args

aDecls :: AExp FO Plain -> Set SMTDecl
aDecls (ALit _) = Set.empty
aDecls (AIdt l) = lDecls l
aDecls (ABinExp _ l r) = Set.union (aDecls l) (aDecls r)
aDecls (ALogVar (Idt v)) = Set.singleton $ SMTConst v
aDecls (AFunCall _ args) = foldl Set.union Set.empty $ map aDecls args
aDecls (AArray _) = error "unsupported array"

lDecls :: LExp FO Plain -> Set SMTDecl
lDecls (LIdt (Idt s)) = Set.singleton $ SMTConst s
lDecls (LArray lExp aExp) = Set.union (lDecls lExp) (aDecls aExp)
lDecls (LStructurePart lExp (Idt accessor)) = Set.insert (SMTUnary accessor) (lDecls lExp)
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