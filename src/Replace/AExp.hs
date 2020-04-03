{-# LANGUAGE GADTs #-}
module Replace.AExp (replaceAExp) where
import AST

type ReplaceAExp a = (LExp FO Refs) -> (AExp FO Refs) -> a -> a

replaceAExp :: ReplaceAExp (BExp FO Refs)
replaceAExp aOld aNew = mapAExps (aReplaceAExp aOld aNew)

aReplaceAExp :: ReplaceAExp (AExp FO Refs)
aReplaceAExp aOld aNew (AIdt aCurrent) | aOld == aCurrent = aNew
aReplaceAExp _ _ (ALit i) = ALit i
aReplaceAExp _ _ (ALogVar v) = ALogVar v
aReplaceAExp aOld aNew (ABinExp op l r) = ABinExp op (aReplaceAExp aOld aNew l) (aReplaceAExp aOld aNew r)
aReplaceAExp aOld aNew (AFunCall name args) = AFunCall name $ map (aReplaceAExp aOld aNew) args
aReplaceAExp aOld aNew (AIdt l) = AIdt $ lReplaceAExp aOld aNew l

rReplaceAExp :: ReplaceAExp (ReadLExp FO)
rReplaceAExp aOld aNew (ReadLExp lNested lSExp) = ReadLExp (sReplaceAExp aOld aNew lNested) (lReplaceAExp aOld aNew lSExp)

sReplaceAExp :: ReplaceAExp State
sReplaceAExp aOld aNew (Update state lExp aExp) = Update
    (sReplaceAExp aOld aNew state) 
    (lReplaceAExp aOld aNew lExp)
    (aReplaceAExp aOld aNew aExp)
sReplaceAExp _ _ s = s

lReplaceAExp :: ReplaceAExp (LExp FO Refs)
lReplaceAExp lOld (AIdt lNew) lCurrent | lOld == lCurrent = lNew
lReplaceAExp aOld aNew (LArray lExp aSExp) = LArray (lReplaceAExp aOld aNew lExp) (aReplaceAExp aOld aNew aSExp)
lReplaceAExp aOld aNew (LStructurePart lExp idt) = LStructurePart (lReplaceAExp aOld aNew lExp) idt
lReplaceAExp aOld aNew (LRead readLExp) = LRead $ rReplaceAExp aOld aNew readLExp
lReplaceAExp _ _ (LIdt idt) = LIdt idt