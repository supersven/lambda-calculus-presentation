
module UntypedSyntax where

type Name = String

data Expr                    -- $e ::= \ \ \ \ \ \ \ \ \ \ \ \text{Expressions:}$
  = Var Name                 --       $x \ \ \  \ \ \ \ \ \ \ \ \text{Variable}$
  | Lambda Name              --       $\lambda x.e \ \ \  \ \ \ \ \text{Abstraction}$
           Expr
  | App Expr                 --       $e \ e \ \ \  \ \ \ \ \ \ \text{Application}$
        Expr
  deriving (Eq, Show)
