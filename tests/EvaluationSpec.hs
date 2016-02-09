module EvaluationSpec (
    spec
) where

import Test.Hspec
import Control.Monad
import Parser
import Evaluation
import qualified StandardTerms
import Syntax

spec :: SpecWith ()
spec = describe "semantics" $ do
    describe "fv" $ do
        it "should return [] for the omega combinator" $
            fv StandardTerms.omega `shouldBe` []

        it "should return [] for the Y combinator" $
            fv StandardTerms.y `shouldBe` []

        it "should return [y] for \\x f.f x y" $
            liftM fv (parse "\\x f.f x y") `shouldBe` Right ["y"]

    describe "subst" $ do
        it "should make a basic substitution" $ 
            subst (Lambda "x" (Apply (Var "f") (Var "x"))) "f" (Lambda "z" (Apply (Var "z") (Var "z"))) `shouldBe`
                (Lambda "x" (Apply (Lambda "z" (Apply (Var "z") (Var "z"))) (Var "x"))) 

        it "should make a very simple variable substitution" $ 
            subst (Var "x") "x" StandardTerms.y `shouldBe` StandardTerms.y

        it "shouldn't do anything if for y[x := N] if x /= y" $ 
            subst (Var "y") "x" StandardTerms.y `shouldBe` (Var "y")

    describe "eval" $ do
        it "should evaluate S K K and return I" $ do
            let i = Lambda "z" (Var "z")
            eval (Apply (Apply StandardTerms.s StandardTerms.k) StandardTerms.k) `shouldBe` i
