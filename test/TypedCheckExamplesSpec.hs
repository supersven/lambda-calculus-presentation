
module TypedCheckExamplesSpec where

import Test.Hspec
import TypedCheck
import TypedSyntax

import qualified Data.Map.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "check" $
    it "should type check these terms" $
--
-- $(\lambda x:Int . x) \ 42 :: Int $
--
     do
      check Map.empty (App (Lambda "x" TInt (Var "x")) (IntValue 5)) 
        `shouldBe` Right TInt

--
-- Does not type check: $(\lambda x:Bool . x) \ 42$
--
      check Map.empty (App (Lambda "x" TBool (Var "x")) (IntValue 5)) 
        `shouldBe` Left "Expected TBool but got : TInt"

--
-- Does not type check: $42 \ False$
--
      check Map.empty (App (IntValue 42) (BoolValue False)) `shouldBe`
        Left "Expected TArr but got : TInt"
