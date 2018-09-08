
module NaiveUntypedEval where

import UntypedSyntax

eval :: Expr -> Expr
eval variable@(Var _) = variable
eval lambda@(Lambda _ _) = lambda
eval (App term1 term2) =
  case eval term1 of
    (Lambda name term1') -> eval $ substitute name term2 term1'
    term -> App term term2

substitute :: String -> Expr -> Expr -> Expr
substitute name substitution var@(Var varName)
  | name  == varName = substitution
  | otherwise = var
substitute name substitution (App term1 term2) =
  App (substitute name substitution term1) (substitute name substitution term2)
substitute name substitution (Lambda varName term) =
  if name == varName
    then Lambda varName term
    else Lambda varName (substitute name substitution term)
