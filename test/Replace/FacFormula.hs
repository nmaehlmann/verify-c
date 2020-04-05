module Replace.FacFormula where
import AST

facFormulaForState :: State -> BExp FO Refs
facFormulaForState state = 
    let fac = Idt $ "fac"
        p = LIdt $ Idt $ "p"
        c = LIdt $ Idt $ "c"
        n = LIdt $ Idt $ "n"
        readFromS var = AIdt $ LRead state var

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
