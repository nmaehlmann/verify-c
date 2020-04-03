{-# LANGUAGE GADTs #-}
module Memory.Unlift (unliftMemory) where
import AST

unliftMemory :: BExp FO Refs -> Maybe (BExp FO Plain)
unliftMemory BTrue = Just BTrue
unliftMemory BFalse = Just BFalse
unliftMemory (BComp op l r) = BComp op <$> unhashmark l <*> unhashmark r
unliftMemory (BNeg b) = BNeg <$> unliftMemory b
unliftMemory (BBinExp op l r) = BBinExp op <$> unliftMemory l <*> unliftMemory r
unliftMemory (BForall i b) = BForall i <$> unliftMemory b
unliftMemory (BExists i b) = BExists i <$> unliftMemory b
unliftMemory (BPredicate name args) = BPredicate name <$> mapM unhashmark args

undagger :: LExp FO Refs -> Maybe (LExp FO Plain)
undagger (LIdt idt) = Just $ LIdt idt
undagger (LArray idt idx) = LArray <$> (undagger idt) <*> (unhashmark idx)
undagger (LStructurePart struct part) = LStructurePart <$> (undagger struct) <*> Just part
undagger (LRead r) = unread r

unread :: ReadLExp FO -> Maybe (LExp FO Plain)
unread (ReadLExp s _) | s /= sigma = Nothing
unread (ReadLExp _ (LIdt i)) = Just $ LIdt i
unread (ReadLExp _ (LRead r)) = LDeref <$> unread r
unread (ReadLExp _ l) = undagger l

unhashmark :: AExp FO Refs -> Maybe (AExp FO Plain)
unhashmark (ALit i) = Just $ ALit i
unhashmark (AIdt (LIdt i)) = Just $ AAddress $ LIdt i
unhashmark (AIdt l) = AIdt <$> undagger l
unhashmark (ABinExp op l r) = ABinExp op <$> (unhashmark l) <*> (unhashmark r)
unhashmark (AFunCall name args) = AFunCall name <$> mapM unhashmark args
unhashmark (ALogVar v) = Just $ ALogVar v
-- unhashmark (AIdt l) = AIdt <$> undagger l