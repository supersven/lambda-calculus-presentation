module UntypedEvalSpec where

import Test.Hspec
import UntypedEval
import UntypedSyntax

import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Vars" $ do
    it "{a = \\x -> x} a => \\x -> x" $
      (eval (Map.singleton "a" (Lambda "x" (Var "x"))) (Var "a")) `shouldBe`
      Just (Lambda "x" (Var "x"))
  describe "Lambdas" $ do
    it "{} \\x -> x => \\x -> x" $
      (eval Map.empty (Lambda "x" (Var "x"))) `shouldBe`
      Just (Lambda "x" (Var "x"))
  describe "Apps" $ do
    it "{} (\\x -> x) a => a" $
      (eval Map.empty (App (Lambda "x" (Var "x")) (Var "a"))) `shouldBe`
      Just (Var "a")
