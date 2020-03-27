{-# LANGUAGE GADTs #-}
module SMTExport where
import AST
import Data.List

bAssert :: BExp FO Plain -> String
bAssert b = sExp ["assert", bToSMT (BNeg b)]

bToSMT :: BExp FO Plain -> String
bToSMT BTrue = "true"
bToSMT BFalse = "false"
bToSMT (BComp op l r) = sExp [show op, aToSMT l, aToSMT r]
bToSMT (BNeg b) = sExp ["not", bToSMT b]
bToSMT (BBinExp op l r) = sExp [binOpToSMT op, bToSMT l, bToSMT r]
bToSMT (BForall i b) = sExp ["forall", "((" ++ show i ++ " Int" ++ "))", bToSMT b]
bToSMT (BExists i b) = sExp ["exists", "((" ++ show i ++ " Int" ++ "))", bToSMT b]
bToSMT (BPredicate name args) = error "unsupported predicate"

binOpToSMT :: BBinOp -> String
binOpToSMT And = "and"
binOpToSMT Or = "or"
binOpToSMT Implies = "implies"

aToSMT :: AExp FO Plain -> String
aToSMT (ALit i) = show i
aToSMT (AIdt l) = lToSMT l
aToSMT (ABinExp op l r) = sExp [show op, aToSMT l, aToSMT r]
aToSMT (ALogVar v) = show v
aToSMT (AArray fields) = error "unsupported array"
aToSMT (AFunCall _ _) = error "unsupported fun call"

lToSMT :: LExp FO Plain -> String
lToSMT (LIdt i) = show i
lToSMT (LArray lExp aExp) = sExp ["select", lToSMT lExp, aToSMT aExp]
lToSMT (LStructurePart lExp idt) = error "unsupported struct part"
lToSMT (LDeref i) = error "unsupported deref"

sExp :: [String] -> String
sExp s = "(" ++ concat (intersperse " " s) ++ ")"