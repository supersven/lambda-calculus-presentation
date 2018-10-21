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
     check (Map.singleton "a" TInt) (Var "a") `shouldBe` Right TInt
    it "{b::TBool} a => TBool" $
     check (Map.singleton "a" TBool) (Var "a") `shouldBe` Right TBool
    it "{} a => FAIL" $
     check Map.empty (Var "a") `shouldBe` Left "Var not found!"

  describe "Lambdas" $ do
    it "{} \\x::TInt -> x => TInt -> TInt" $
      check Map.empty (Lambda "x" TInt (Var "x")) `shouldBe` Right (TArr TInt TInt)
    it "{} \\x::TInt -> True => TInt -> TBool" $
      check Map.empty (Lambda "x" TInt (BoolValue True)) `shouldBe` Right (TArr TInt TBool)
    it "{} \\x::TInt -> x => TInt -> (TBool -> TInt)" $
      check Map.empty (Lambda "x" TInt (Lambda "y" TBool (Var "x"))) `shouldBe` Right (TArr TInt (TArr TBool TInt))

  describe "Applications" $ do
    it "{} (\\x::TInt -> x) 42 => TInt" $
      check Map.empty (App (Lambda "x" TInt (Var "x")) (IntValue 5)) `shouldBe` Right TInt
    it "{} (\\x::TInt -> x) False => FAIL" $
      check Map.empty (App (Lambda "x" TInt (Var "x")) (BoolValue False)) `shouldBe` Left "Expected TInt but got : TBool"
    it "{} 42 False => FAIL" $
      check Map.empty (App (IntValue 42) (BoolValue False)) `shouldBe` Left "Expected TArr but got : TInt"
