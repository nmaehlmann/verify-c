{-# LANGUAGE GADTs #-}
module LiftMemory where
import AST

bLiftMemory :: BExp FO Plain -> BExp FO Refs
bLiftMemory = mapAExps hashmark

dagger :: LExp FO Plain -> LExp FO Refs
dagger (LIdt idt) = LIdt idt
dagger (LArray idt idx) = LArray (dagger idt) (hashmark idx)
dagger (LStructurePart struct part) = LStructurePart (dagger struct) part
dagger (LDeref idt) = LRead $ ReadLExp sigma $ dagger idt

hashmark :: AExp FO Plain -> AExp FO Refs
hashmark (ALit i) = ALit i
hashmark (AIdt l) = ARead $ ReadLExp sigma $ dagger l
hashmark (ABinExp op l r) = ABinExp op (hashmark l) (hashmark r)
hashmark (AArray fields) = AArray $ map hashmark fields
hashmark (AFunCall name args) = AFunCall name $ map hashmark args
hashmark (ALogVar v) = ALogVar v