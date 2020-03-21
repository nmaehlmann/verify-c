module IT.VCSpec where
import Test.Hspec
import AST
import VC
import IT.FacFormula

awpSpec :: IO Spec
awpSpec = return $ describe "VC.awp" $ do
    it "it generates the precondition of an assertion of the fac program" $ do
        let c = LIdt $ Idt $ "c"
        let facFormula = facFormulaForState sigma
        let assnCPlusTwoToC = Assignment c $ ABinExp Add (AIdt c) $ ALit 2

        -- read(s, c) + 2
        let sC = LIdt $ Idt $ "c"
        let cPlus2 = ABinExp Add (ARead (ReadLExp sigma sC)) $ ALit 2

        -- upd(s,c, read(c,s) + 1)
        let updatedState = Update sigma sC cPlus2
        let facFormulaUpdated = facFormulaForState updatedState
        awp assnCPlusTwoToC facFormula BTrue  `shouldBe` facFormulaUpdated
        






    