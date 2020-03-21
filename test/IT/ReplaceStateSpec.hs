module IT.ReplaceStateSpec where
import Test.Hspec
import AST
import ReplaceState
import IT.FacFormula

replaceStateSpec :: IO Spec
replaceStateSpec = return $ describe "ReplaceState.bReplaceState" $ do
    it "replaces c with c + 1 in an assertion of the fac program" $ do
        let facFormula = facFormulaForState sigma
    
        -- read(s, c) + 1
        let c = LIdt $ Idt $ "c"
        let cPlus1 = ABinExp Add (ARead (ReadLExp sigma c)) $ ALit 1

        -- upd(s, c, read(c,s) + 1)
        let updatedState = Update sigma c cPlus1
        
        let facFormulaUpdated = facFormulaForState updatedState
        bReplaceState sigma updatedState facFormula `shouldBe` facFormulaUpdated