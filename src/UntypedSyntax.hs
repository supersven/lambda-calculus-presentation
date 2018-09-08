
module UntypedSyntax where

type Name = String

data Expr
  = Var Name
  | App Expr
        Expr
  | Lambda Name
           Expr
  deriving (Eq, Show)
