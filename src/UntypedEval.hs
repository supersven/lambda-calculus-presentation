
module UntypedEval where

import UntypedSyntax

import qualified Data.Map.Strict as Map

type Environment = Map.Map Name Expr

eval :: Environment -> Expr -> Maybe Expr
eval env (Var name) = find env name
eval env (App term1 term2) = case eval env term1 of
  Just (Lambda name term) -> eval (Map.insert name term2 env) term
  Just term                    -> Just (App term term2)
  Nothing -> Nothing
eval env lambda@(Lambda _ _) = Just lambda

find ::  Environment -> Name -> Maybe Expr
find env name = Map.lookup name env
