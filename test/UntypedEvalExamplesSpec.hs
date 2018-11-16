
module UntypedEvalExamplesSpec where

import NaiveUntypedEval
import Prelude hiding (and)
import Test.Hspec
import UntypedSyntax
import UntypedSyntaxExamples

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "eval" $
    it "should evaluate these terms"  $ do
--
-- $a \to a $
--
      eval (Var "a") `shouldBe` Var "a"

--
-- $true \equiv \lambda p . \lambda q . p$ 
--
-- $true \ a \ b \to a$
--
      eval (App (App true (Var "a")) (Var "b")) `shouldBe` Var "a"

--
-- $false \equiv \lambda p . \lambda q . q$
--
-- $and \equiv \lambda p . \lambda q . p \ q \ p$
--
-- $and \ true \ false \to false$
--
      eval (App (App and true) false) `shouldBe`
        Lambda "p" (Lambda "q" (Var "q"))
