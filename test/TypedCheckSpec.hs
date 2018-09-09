module TypedCheckSpec where

import Test.Hspec
import TypedCheck
import TypedSyntax

import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Vars" $ do
    it "{a::TInt} a => TInt" $
     (check (Map.singleton "a" TInt) (Var "a")) `shouldBe` (Right TInt)
    it "{} a => FAIL" $
     (check Map.empty (Var "a")) `shouldBe` (Left "Var not found!")

  describe "Lambdas" $ do
    it "{} \\x::TInt -> x => TInt -> TInt" $
      (check Map.empty (Lambda "x" TInt (Var "x"))) `shouldBe` (Right (TArr TInt TInt))
    it "{} \\x::TInt -> x => TInt -> (TBool -> TInt)" $
      (check Map.empty (Lambda "x" TInt (Lambda "y" TBool (Var "x")))) `shouldBe` (Right (TArr TInt (TArr TBool TInt)))

  describe "Apps" $ do
    it "" $
      (check Map.empty (Lambda "x" TInt (Var "x"))) `shouldBe` (Right (TArr TInt TInt))
