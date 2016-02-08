module ParserSpec (
    spec
) where

import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Parser
import Syntax

spec :: SpecWith ()
spec = describe "parse" $ do
    it "should parse the omega combinator" $
        parse "\\x.x x" `shouldBe` Right (Lambda "x" (Apply (Var "x") (Var "x")))

    it "should parse \\x.\\f. (f x x) (used to have a bug)" $
        parse "\\x.\\f. (f x x)" `shouldBe` 
            Right (Lambda "x" (Lambda "f" (Apply (Apply (Var "f") (Var "x")) (Var "x"))))

    let conv = map (\c -> if c == '\x03bb' then '\\' else c)
    it "should parse a randomly generated expression tree" $ property $
        \e -> parse (conv . show $ (e::LExpr)) `shouldBe` Right e

instance Arbitrary LExpr where
    arbitrary = oneof [
        liftM Var arbitraryVariable
        , liftM2 Lambda arbitraryVariable arbitrary
        , liftM2 Apply arbitrary arbitrary
        ] 

arbitraryVariable :: Gen String
arbitraryVariable = do
    i <- choose (1, 2) 
    l <- sequence $ [elements (['a'..'z'] ++ ['A'..'Z']) | _ <- [1..i::Int]]
    n <- arbitrary
    return $ l ++ show (abs (n::Int))
