{-# LANGUAGE GADTs #-}
module LiftMemory where
import AST

bLiftMemory :: BExpr FO Plain -> BExpr FO Refs
bLiftMemory = mapAExps hashmark

dagger :: LExpr FO Plain -> LExpr FO Refs
dagger (LIdt idt) = LIdt idt
dagger (LArray idt idx) = LArray (dagger idt) (hashmark idx)
dagger (LStructurePart struct part) = LStructurePart (dagger struct) part
dagger (LDeref idt) = LRead $ ReadLExpr sigma $ dagger idt

hashmark :: AExpr FO Plain -> AExpr FO Refs
hashmark (ALit i) = ALit i
hashmark (AIdt l) = ARead $ ReadLExpr sigma $ dagger l
hashmark (ABinExp op l r) = ABinExp op (hashmark l) (hashmark r)
hashmark (AArray fields) = AArray $ map hashmark fields
hashmark (AFunCall name args) = AFunCall name $ map hashmark args
hashmark (ALogVar v) = ALogVar v