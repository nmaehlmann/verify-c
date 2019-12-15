import Test.Hspec
import Test.QuickCheck
import Text.Parsec
import Data.Either

import AST
import Parser.ArithmeticExpression
import Parser.BooleanExpression
import Parser.Statement

insertWhiteSpaces :: [String] -> Gen String
insertWhiteSpaces [] = return ""
insertWhiteSpaces (x:xs) = (\spacedXs -> x ++ spacedXs) <$> insertWhiteSpaces' xs
insertWhiteSpaces' [] = spacesGen
insertWhiteSpaces' (x:xs) = do
    s <- spacesGen
    next <- insertWhiteSpaces xs
    return $ s ++ x ++ next

spacesGen :: Gen String
spacesGen = do
    numOfSpaces <- (arbitrary :: Gen Int) `suchThat` (>= 0)
    return $ replicate numOfSpaces ' '

whitespaceIndependent s f = property $ forAll (insertWhiteSpaces s) $ f

main :: IO ()
main = hspec $ do
    describe "Parser.BooleanExpression" $ do
        let parseBExp t = parse bExp "" t
        let x = AIdt $ Idt "x"
        let y = AIdt $ Idt "y"
        let z = AIdt $ Idt "z"
        let isValid = isRight

        it "parses boolean constants" $ do
            parseBExp "true" `shouldBe` Right BTrue
            parseBExp "false" `shouldBe` Right BFalse
            parseBExp "maybe" `shouldNotSatisfy` isValid

        it "parses simple comparisons" $ do
            parseBExp "x < y" `shouldBe` (Right (BLess x y))
            parseBExp "x = y" `shouldBe` (Right (BEq x y))

        it "ignores whitespaces in '<' comparisons" $ do
            whitespaceIndependent ["x","<","y"] $ \s -> parseBExp s `shouldBe` (Right (BLess x y))
        
        it "ignores whitespaces in '=' comparisons" $ do
            whitespaceIndependent ["x","=","y"] $ \s -> parseBExp s `shouldBe` (Right (BEq x y))

        it "parses simple conjunctions" $ do
            parseBExp "x < y && y < z" `shouldBe` (Right (BAnd (BLess x y) (BLess y z)))

        it "parses simple disjunctions" $ do
            parseBExp "x < y || y < z" `shouldBe` (Right (BOr (BLess x y) (BLess y z)))

        it "ignores whitespaces in conjunctions" $ do
            whitespaceIndependent ["x < y", "&&", "y < z"] $ 
                \s -> parseBExp s `shouldBe` (Right (BAnd (BLess x y) (BLess y z)))
    
        it "ignores whitespaces in disjunctions" $ do
            whitespaceIndependent ["x < y", "||", "y < z"] $ 
                \s -> parseBExp s `shouldBe` (Right (BOr (BLess x y) (BLess y z)))

    describe "Parser.Statement" $ do
        let parseStmt t = parse statement "" t
        let x = Idt "x"
        let y = Idt "y"

        it "parses assignments" $ do
            parseStmt "x = y;" `shouldBe` (Right (Assignment x (AIdt y)))

        it "ignores whitespaces in assignments" $ do
            whitespaceIndependent ["x", "=", "y", ";"] $ 
                \s -> parseStmt s  `shouldBe` (Right (Assignment x (AIdt y)))

    describe "Parser.ArithmeticExpression" $ do
        let parseAExp t = parse aExp "" t
        let x = AIdt $ Idt "x"
        let y = AIdt $ Idt "y"

        it "parses additions" $ do
            parseAExp "x + y;" `shouldBe` (Right (ABinExp Add x y))

        it "ignores whitespaces in additions" $ do
            whitespaceIndependent ["x", "+", "y"] $ 
                \s -> parseAExp s  `shouldBe` (Right (ABinExp Add x y))

