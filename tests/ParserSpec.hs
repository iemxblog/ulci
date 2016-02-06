module ParserSpec (
    spec
) where

import Test.Hspec
import Parser
import Syntax

spec :: SpecWith ()
spec = describe "parse" $ do
    it "should parse the omega combinator" $
        parse "\\x.x x" `shouldBe` Right (Lambda "x" (Apply (Var "x") (Var "x")))

    it "should parse \\x.\\f. (f x x) (used to have a bug)" $
        parse "\\x.\\f. (f x x)" `shouldBe` 
            Right (Lambda "x" (Lambda "f" (Apply (Apply (Var "f") (Var "x")) (Var "x"))))
