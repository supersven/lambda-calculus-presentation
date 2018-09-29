module UntypedEvalSpec where

import NaiveUntypedEval
import UntypedSyntax
import UntypedSyntaxExamples

import Test.Hspec

import Prelude hiding (and)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Variables" $ do it "a => a" $ do eval (Var "a") `shouldBe` (Var "a")
  describe "Lambdas" $ do
    it
      "No evaluation inside lambdas: \\x -> (\\y -> y) a =>  \\x -> (\\y -> y) a" $ do
      eval (Lambda "x" (App (Lambda "y" (Var "y")) (Var "a"))) `shouldBe`
        (Lambda "x" (App (Lambda "y" (Var "y")) (Var "a")))
  describe "Applications" $ do
    it "(\\x -> x) a => a" $ do
      eval (App (Lambda "x" (Var "x")) (Var "a")) `shouldBe` (Var "a")
  describe "Church Booleans" $ do
    it "true a b => a" $ do
      eval (App (App true (Var "a")) (Var "b")) `shouldBe` Var "a"
    it "false a b => b" $ do
      eval (App (App false (Var "a")) (Var "b")) `shouldBe` Var "b"
    it "and true true a b => a" $ do
      eval (App (App (App (App and true) true) (Var "a")) (Var "b")) `shouldBe`
        Var "a"
    it "and true false a b => b" $ do
      eval (App (App (App (App and true) false) (Var "a")) (Var "b")) `shouldBe`
        Var "b"
