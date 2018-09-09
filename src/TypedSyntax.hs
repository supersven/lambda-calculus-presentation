
module TypedSyntax where

import qualified Data.Map.Strict as Map

type Name = String

type Environment = Map.Map Name Type

data Type
  = TInt
  | TBool
  | TArr Type
         Type
  deriving (Eq, Show)

data Expr
  = IntValue Int
  | BoolValue Bool
  | Var Name
  | App Expr
        Expr
  | Lambda Name
           Type
           Expr
  deriving (Eq, Show)
