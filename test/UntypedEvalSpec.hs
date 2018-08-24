module UntypedEvalSpec where

import Test.Hspec
import UntypedEval

import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Variables" $ do
    it "{a = \\x -> x} a => \\x -> x" $
     (eval (Map.singleton "a" (Abstraction "x" (Variable "x"))) (Variable "a")) `shouldBe` Just (Abstraction "x" (Variable "x"))

  describe "Abstractions" $ do
    it "{} \\x -> x => \\x -> x" $
     (eval Map.empty (Abstraction "x" (Variable "x"))) `shouldBe` Just (Abstraction "x" (Variable "x"))

  describe "Applications" $ do
    it "{} (\\x -> x) a => a" $
     (eval Map.empty (Application (Abstraction "x" (Variable "x")) (Variable "a"))) `shouldBe` Just (Variable "a")

