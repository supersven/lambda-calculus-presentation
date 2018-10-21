
module TypedSyntax where

import qualified Data.Map.Strict as Map

type Name = String
type Error = String

data Expr                    -- $e ::= \quad \quad \quad \quad \quad \quad \quad \quad Expressions:$
  = IntValue Int             --     $[-2^{29} .. 2^{29}-1] \quad \quad \quad \quad \quad \text{Integer Literal}$
  | BoolValue Bool           --     $True \ | \ False \quad \quad \quad \quad \quad \quad \ \text{Boolean Literal}$
  | Var Name                 --     $x \quad \quad \quad \quad \quad \quad \quad \quad \quad \quad \quad \ \text{Variable}$
  | App Expr                 --     $e \ e \quad \quad \quad \quad \quad \quad \quad \quad \quad \quad \ \text{Application}$
        Expr
  | Lambda Name              --     $\lambda x \mathbf{:\tau}.e \quad \quad \quad \quad \quad \quad \quad \quad \ \text{Abstraction}$
           Type
           Expr
  deriving (Eq, Show)

type Environment = Map.Map Name Type

data Type                    -- $\tau ::= \quad \quad \quad \quad \quad \quad Types:$
  = TInt                     --     $Int \quad \quad \quad \quad \quad \quad \text{Integer}$
  | TBool                    --     $Bool \quad \quad \quad \quad \quad \ \text{Boolean}$
  | TArr Type                --     $\tau_1 \to \tau_2 \quad \quad \quad \ \ \text{Abstraction / Function}$
         Type
  deriving (Eq, Show)
