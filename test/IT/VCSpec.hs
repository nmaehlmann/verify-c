module IT.VCSpec where

import Test.Hspec

import AST
import VC

daggerSpec :: IO Spec
daggerSpec = return $ describe "VC.dagger" $ do
    let x = Idt "x"

    it "resolves references" $ do
        let derefX = LDereference $ LIdt x
        let derefedX = LSRead $ ReadLExp sigma $ LSIdt x
        dagger derefX `shouldBe` derefedX
    
    it "resolves references to references" $ do
        let doublyDerefX = LDereference $ LDereference $ LIdt x
        let doublyDerefedX = LSRead $ ReadLExp sigma $ LSRead $ ReadLExp sigma $ LSIdt x
        dagger doublyDerefX `shouldBe` doublyDerefedX

hashmarkSpec :: IO Spec
hashmarkSpec = return $ describe "VC.hashmark" $ do
    let x = Idt "x"

    it "resolves regular variables" $ do
        -- x
        let justX = AIdt $ LIdt x
        let readX = ASRead $ ReadLExp sigma $ LSIdt x
        hashmark justX `shouldBe` readX

    it "resolves references" $ do
        -- *x
        let derefX = AIdt $ LDereference $ LIdt x
        let derefedX = ASRead $ ReadLExp sigma $ LSRead $ ReadLExp sigma $ LSIdt x
        hashmark derefX `shouldBe` derefedX

    it "resolves arrays" $ do
        -- *x[1]
        let derefX = LDereference $ LIdt x
        let array = AIdt $ LArray derefX $ ALit 1
        let derefedX = LSRead $ ReadLExp sigma $ LSIdt x
        let derefedArray = ASRead $ ReadLExp sigma $ LSArray derefedX $ ASLit 1
        hashmark array `shouldBe` derefedArray

    it "resolves addition of two arrays" $ do
        let derefX = LDereference $ LIdt x
        let array = AIdt $ LArray derefX $ ALit 1
        let derefedX = LSRead $ ReadLExp sigma $ LSIdt x
        let derefedArray = ASRead $ ReadLExp sigma $ LSArray derefedX $ ASLit 1
        let addition = ABinExp Add array array
        let derefedAddition = ASBinExp Add derefedArray derefedArray
        hashmark addition `shouldBe` derefedAddition

replaceStateSpec :: IO Spec
replaceStateSpec = return $ describe "VC.replaceState" $ do
    it "replaces c with c + 1 in an assertion of the fac program" $ do
        let facFormula = facFormulaForState sigma
    
        -- read(c,s) + 1
        let c = LSIdt $ Idt $ "c"
        let cPlus1 = ASBinExp Add (ASRead (ReadLExp sigma c)) $ ASLit 1

        -- upd(s,c, read(c,s) + 1)
        let updatedState = Update sigma c cPlus1
        
        let facFormulaUpdated = facFormulaForState updatedState
        replaceState sigma updatedState facFormula `shouldBe` facFormulaUpdated

awpSpec :: IO Spec
awpSpec = return $ describe "VC.awp" $ do
    it "it generates the precondition of an assertion of the fac program" $ do
        let c = LIdt $ Idt $ "c"
        let facFormula = facFormulaForState sigma
        let assnCPlusTwoToC = Assignment c $ ABinExp Add (AIdt c) $ ALit 2

        -- read(c,s) + 2
        let sC = LSIdt $ Idt $ "c"
        let cPlus2 = ASBinExp Add (ASRead (ReadLExp sigma sC)) $ ASLit 2

        -- upd(s,c, read(c,s) + 1)
        let updatedState = Update sigma sC cPlus2
        let facFormulaUpdated = facFormulaForState updatedState
        awp assnCPlusTwoToC facFormula FOTrue  `shouldBe` facFormulaUpdated
        

facFormulaForState :: State -> BExprFO
facFormulaForState state = 
    let fac = Idt $ "fac"
        p = LSIdt $ Idt $ "p"
        c = LSIdt $ Idt $ "c"
        n = LSIdt $ Idt $ "n"
        readFromS var = ASRead $ ReadLExp state var

        -- read(s, p)
        readP = readFromS p

        -- fac(read(c, s) - 1)
        readC = readFromS c
        cMinus1 = ASBinExp Sub readC (ASLit 1)
        facCMinus1 = ASFunCall fac [cMinus1]

        -- read(p, s) = fac(read(c, s) - 1)
        pEqFacCMinus1 = FOComp Equal readP facCMinus1

        -- read(n, s) + 1
        readN = readFromS n
        nPlus1 = ASBinExp Add readN (ASLit 1)

        -- read(c, s) <= read(n, s) + 1
        cLeqNPlus1 = FOComp LessOrEqual readC nPlus1

    -- read(p, s) = fac(read(c, s) - 1) and read(c, s) <= read(n, s) + 1
    in  FOBinExp And pEqFacCMinus1 cLeqNPlus1





    