module IT.VCSpec where

import Test.Hspec

import AST
import VC
import LiftMemory
import ReplaceState

daggerSpec :: IO Spec
daggerSpec = return $ describe "VC.dagger" $ do
    let x = Idt "x"

    it "resolves references" $ do
        let derefX = LDeref $ LIdt x
        let derefedX = LRead $ ReadLExp sigma $ LIdt x
        dagger derefX `shouldBe` derefedX
    
    it "resolves references to references" $ do
        let doublyDerefX = LDeref $ LDeref $ LIdt x
        let doublyDerefedX = LRead $ ReadLExp sigma $ LRead $ ReadLExp sigma $ LIdt x
        dagger doublyDerefX `shouldBe` doublyDerefedX

hashmarkSpec :: IO Spec
hashmarkSpec = return $ describe "VC.hashmark" $ do
    let x = Idt "x"

    it "resolves regular variables" $ do
        -- x
        let justX = AIdt $ LIdt x
        let readX = ARead $ ReadLExp sigma $ LIdt x
        hashmark justX `shouldBe` readX

    it "resolves references" $ do
        -- *x
        let derefX = AIdt $ LDeref $ LIdt x
        let derefedX = ARead $ ReadLExp sigma $ LRead $ ReadLExp sigma $ LIdt x
        hashmark derefX `shouldBe` derefedX

    it "resolves arrays" $ do
        -- *x[1]
        let derefX = LDeref $ LIdt x
        let array = AIdt $ LArray derefX $ ALit 1
        let derefedX = LRead $ ReadLExp sigma $ LIdt x
        let derefedArray = ARead $ ReadLExp sigma $ LArray derefedX $ ALit 1
        hashmark array `shouldBe` derefedArray

    it "resolves addition of two arrays" $ do
        let derefX = LDeref $ LIdt x
        let array = AIdt $ LArray derefX $ ALit 1
        let derefedX = LRead $ ReadLExp sigma $ LIdt x
        let derefedArray = ARead $ ReadLExp sigma $ LArray derefedX $ ALit 1
        let addition = ABinExp Add array array
        let derefedAddition = ABinExp Add derefedArray derefedArray
        hashmark addition `shouldBe` derefedAddition

replaceStateSpec :: IO Spec
replaceStateSpec = return $ describe "VC.replaceState" $ do
    it "replaces c with c + 1 in an assertion of the fac program" $ do
        let facFormula = facFormulaForState sigma
    
        -- read(c,s) + 1
        let c = LIdt $ Idt $ "c"
        let cPlus1 = ABinExp Add (ARead (ReadLExp sigma c)) $ ALit 1

        -- upd(s,c, read(c,s) + 1)
        let updatedState = Update sigma c cPlus1
        
        let facFormulaUpdated = facFormulaForState updatedState
        bReplaceState sigma updatedState facFormula `shouldBe` facFormulaUpdated

awpSpec :: IO Spec
awpSpec = return $ describe "VC.awp" $ do
    it "it generates the precondition of an assertion of the fac program" $ do
        let c = LIdt $ Idt $ "c"
        let facFormula = facFormulaForState sigma
        let assnCPlusTwoToC = Assignment c $ ABinExp Add (AIdt c) $ ALit 2

        -- read(c,s) + 2
        let sC = LIdt $ Idt $ "c"
        let cPlus2 = ABinExp Add (ARead (ReadLExp sigma sC)) $ ALit 2

        -- upd(s,c, read(c,s) + 1)
        let updatedState = Update sigma sC cPlus2
        let facFormulaUpdated = facFormulaForState updatedState
        awp assnCPlusTwoToC facFormula BTrue  `shouldBe` facFormulaUpdated
        

facFormulaForState :: State -> BExp FO Refs
facFormulaForState state = 
    let fac = Idt $ "fac"
        p = LIdt $ Idt $ "p"
        c = LIdt $ Idt $ "c"
        n = LIdt $ Idt $ "n"
        readFromS var = ARead $ ReadLExp state var

        -- read(s, p)
        readP = readFromS p

        -- fac(read(c, s) - 1)
        readC = readFromS c
        cMinus1 = ABinExp Sub readC (ALit 1)
        facCMinus1 = AFunCall fac [cMinus1]

        -- read(p, s) = fac(read(c, s) - 1)
        pEqFacCMinus1 = BComp Equal readP facCMinus1

        -- read(n, s) + 1
        readN = readFromS n
        nPlus1 = ABinExp Add readN (ALit 1)

        -- read(c, s) <= read(n, s) + 1
        cLeqNPlus1 = BComp LessOrEqual readC nPlus1

    -- read(p, s) = fac(read(c, s) - 1) and read(c, s) <= read(n, s) + 1
    in  BBinExp And pEqFacCMinus1 cLeqNPlus1





    