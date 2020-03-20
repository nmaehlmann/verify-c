{-# LANGUAGE GADTs #-}
module LiftLogic where
import AST

bLiftLogic :: BExpr C0 Plain -> BExpr FO Plain
bLiftLogic = mapAExps aLiftLogic

aLiftLogic :: AExpr C0 Plain -> AExpr FO Plain
aLiftLogic (ALit i) = ALit i
aLiftLogic (AIdt l) = AIdt $ lLiftLogic l
aLiftLogic (ABinExp op l r) = ABinExp op (aLiftLogic l) (aLiftLogic r)
aLiftLogic (AArray fields) = AArray $ fmap aLiftLogic fields

lLiftLogic :: LExpr C0 Plain -> LExpr FO Plain
lLiftLogic (LIdt i) = LIdt i
lLiftLogic (LArray lExp aExp) = LArray (lLiftLogic lExp) (aLiftLogic aExp)
lLiftLogic (LStructurePart lExp idt) = LStructurePart (lLiftLogic lExp) idt
lLiftLogic (LDeref i) = LDeref $ lLiftLogic i