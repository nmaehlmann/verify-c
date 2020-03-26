{-# LANGUAGE GADTs #-}
module UnliftMemory where
import AST

bUnliftMemory :: BExp FO Refs -> Maybe (BExp FO Plain)
bUnliftMemory BTrue = Just BTrue
bUnliftMemory BFalse = Just BFalse
bUnliftMemory (BComp op l r) = BComp op <$> unhashmark l <*> unhashmark r
bUnliftMemory (BNeg b) = BNeg <$> bUnliftMemory b
bUnliftMemory (BBinExp op l r) = BBinExp op <$> bUnliftMemory l <*> bUnliftMemory r
bUnliftMemory (BForall i b) = BForall i <$> bUnliftMemory b
bUnliftMemory (BExists i b) = BExists i <$> bUnliftMemory b
bUnliftMemory (BPredicate name args) = BPredicate name <$> mapM unhashmark args

undagger :: LExp FO Refs -> Maybe (LExp FO Plain)
undagger (LIdt idt) = Just $ LIdt idt
undagger (LArray idt idx) = LArray <$> (undagger idt) <*> (unhashmark idx)
undagger (LStructurePart struct part) = LStructurePart <$> (undagger struct) <*> Just part
undagger (LRead r) = unread r

unread :: ReadLExp FO -> Maybe (LExp FO Plain)
unread (ReadLExp s _) | s /= sigma = Nothing
unread (ReadLExp _ (LIdt i)) = Just $ LIdt i
unread (ReadLExp _ l) = LDeref <$> undagger l

unhashmark :: AExp FO Refs -> Maybe (AExp FO Plain)
unhashmark (ALit i) = Just $ ALit i
unhashmark (ARead r) = AIdt <$> unread r
unhashmark (ABinExp op l r) = ABinExp op <$> (unhashmark l) <*> (unhashmark r)
unhashmark (AArray fields) = AArray <$> mapM unhashmark fields
unhashmark (AFunCall name args) = AFunCall name <$> mapM unhashmark args
unhashmark (ALogVar v) = Just $ ALogVar v