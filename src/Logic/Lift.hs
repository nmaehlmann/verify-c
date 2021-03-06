{-# LANGUAGE GADTs #-}
module Logic.Lift (liftLogic, aLiftLogic, lLiftLogic) where
import AST

liftLogic :: BExp C0 Plain -> BExp FO Plain
liftLogic = mapAExps aLiftLogic

aLiftLogic :: AExp C0 Plain -> AExp FO Plain
aLiftLogic (ALit i) = ALit i
aLiftLogic (AIdt l) = AIdt $ lLiftLogic l
aLiftLogic (ABinExp op l r) = ABinExp op (aLiftLogic l) (aLiftLogic r)
aLiftLogic (AAddress a) = AAddress $ lLiftLogic a

lLiftLogic :: LExp C0 Plain -> LExp FO Plain
lLiftLogic (LIdt i) = LIdt i
lLiftLogic (LArray lExp aExp) = LArray (lLiftLogic lExp) (aLiftLogic aExp)
lLiftLogic (LStructurePart lExp idt) = LStructurePart (lLiftLogic lExp) idt
lLiftLogic (LDeref i) = LDeref $ lLiftLogic i