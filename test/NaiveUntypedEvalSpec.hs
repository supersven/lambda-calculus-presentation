module NaiveUntypedEvalSpec where

import NaiveUntypedEval

import Test.Hspec

import Prelude hiding (and)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Variables" $ do
    it "a => a" $ do
      eval (Variable "a") `shouldBe` (Variable "a")

  describe "Abstractions" $ do
    it "No evaluation inside abstractions: \\x -> (\\y -> y) a =>  \\x -> (\\y -> y) a" $ do
      eval (Abstraction "x" (Application (Abstraction "y" (Variable "y")) (Variable "a"))) `shouldBe` (Abstraction "x" (Application (Abstraction "y" (Variable "y")) (Variable "a")))

  describe "Applications" $ do
    it "(\\x -> x) a => a" $ do
      eval (Application (Abstraction "x" (Variable "x")) (Variable "a")) `shouldBe` (Variable "a")

  describe "Church Booleans" $ do
    it "true a b => a" $ do
      eval (Application (Application true (Variable "a")) (Variable "b")) `shouldBe` Variable "a"
    it "and true true a b => a" $ do
      eval (Application (Application (Application (Application and true) true) (Variable "a")) (Variable "b")) `shouldBe` Variable "a"
    it "and true false a b => b" $ do
      eval (Application (Application (Application (Application and true) false) (Variable "a")) (Variable "b")) `shouldBe` Variable "b"

true:: Term
true = Abstraction "x" (Abstraction "y" (Variable "x"))

false :: Term
false = Abstraction "x" (Abstraction "y" (Variable "y"))

and :: Term
and = Abstraction "p" $ Abstraction "q" $ (Application (Application (Variable "p") (Variable "q")) (Variable "p"))
