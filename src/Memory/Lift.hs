{-# LANGUAGE GADTs #-}
module Memory.Lift (liftMemory, dagger, hashmark) where
import AST

liftMemory :: BExp FO Plain -> BExp FO Refs
liftMemory = mapAExps hashmark

dagger :: LExp FO Plain -> LExp FO Refs
dagger (LIdt idt) = LIdt idt
dagger (LArray idt idx) = LArray (dagger idt) (hashmark idx)
dagger (LStructurePart struct part) = LStructurePart (dagger struct) part
dagger (LDeref idt) = LRead sigma $ dagger idt

hashmark :: AExp FO Plain -> AExp FO Refs
hashmark (ALit i) = ALit i
hashmark (AIdt l) = AIdt $ LRead sigma $ dagger l
hashmark (ABinExp op l r) = ABinExp op (hashmark l) (hashmark r)
hashmark (AFunCall name args) = AFunCall name $ map hashmark args
hashmark (ALogVar v) = ALogVar v
hashmark (AAddress l) = AIdt $ dagger l