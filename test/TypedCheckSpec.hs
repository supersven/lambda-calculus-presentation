module TypedCheckSpec where

import Test.Hspec
import TypedCheck

import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Variables" $ do
    it "{a::TInt} a => TInt" $
     (check (Map.singleton "a" TInt) (Variable "a")) `shouldBe` (Right TInt)
    it "{} a => FAIL" $
     (check Map.empty (Variable "a")) `shouldBe` (Left "Variable not found!")

  describe "Abstractions" $ do
    it "{} \\x::TInt -> x => TInt -> TInt" $
      (check Map.empty (Abstraction "x" TInt (Variable "x"))) `shouldBe` (Right (TArr TInt TInt))
    it "{} \\x::TInt -> x => TInt -> (TBool -> TInt)" $
      (check Map.empty (Abstraction "x" TInt (Abstraction "y" TBool (Variable "x")))) `shouldBe` (Right (TArr TInt (TArr TBool TInt)))

  describe "Applications" $ do
    it "" $
      (check Map.empty (Abstraction "x" TInt (Variable "x"))) `shouldBe` (Right (TArr TInt TInt))
