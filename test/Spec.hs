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
            parseBExp "x == y" `shouldBe` (Right (BEq x y))

        it "ignores whitespaces in '<' comparisons" $ do
            whitespaceIndependent ["x","<","y"] $ \s -> parseBExp s `shouldBe` (Right (BLess x y))
        
        it "ignores whitespaces in '=' comparisons" $ do
            whitespaceIndependent ["x","==","y"] $ \s -> parseBExp s `shouldBe` (Right (BEq x y))

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

        it "parses parentheses" $ do
            parseBExp "(x < y) || ((y < z) && true)" `shouldBe` (Right (BOr (BLess x y) (BAnd (BLess y z) BTrue)))

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
        let z = AIdt $ Idt "z"

        it "parses literals" $ do
            parseAExp "1337" `shouldBe` (Right (ALit 1337))

        it "parses negative literals" $ do
            parseAExp "-1337" `shouldBe` (Right (ALit (-1337)))

        it "parses arithmetic operators" $ do
            parseAExp "1 + 2" `shouldBe` (Right (ABinExp Add (ALit 1) (ALit 2)))
            parseAExp "x - y" `shouldBe` (Right (ABinExp Sub x y))
            parseAExp "x * y" `shouldBe` (Right (ABinExp Mul x y))
            parseAExp "x / y" `shouldBe` (Right (ABinExp Div x y))

        it "ignores whitespaces" $ do
            whitespaceIndependent ["x", "+", "y"] $ 
                \s -> parseAExp s  `shouldBe` (Right (ABinExp Add x y))

        it "parses addition left associative" $ do
            parseAExp "x + y + z" `shouldBe` (Right (ABinExp Add (ABinExp Add x y) z))

        it "parses multiplication left associative" $ do
            parseAExp "x * y * z" `shouldBe` (Right (ABinExp Mul (ABinExp Mul x y) z))

        it "parses multiplication with higher precedence than addition" $ do
            parseAExp "x + y * z" `shouldBe` (Right (ABinExp Add  x (ABinExp Mul y z)))

        it "parses nested parentheses" $ do
            parseAExp "x - (3 / (y + z))" `shouldBe` (Right (ABinExp Sub  x (ABinExp Div (ALit 3) (ABinExp Add y z))))

        it "parses parentheses with higher precedence than multiplication" $ do
            parseAExp "x * (y - z)" `shouldBe` (Right (ABinExp Mul  x (ABinExp Sub y z)))

