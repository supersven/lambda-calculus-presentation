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
    it "" $
     True `shouldBe` True

  describe "Applications" $ do
    it "" $
     True `shouldBe` True

