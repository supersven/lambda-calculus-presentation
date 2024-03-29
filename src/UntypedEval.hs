
module UntypedEval where

import UntypedSyntax

eval :: Expr -> Expr
-- No rule for variables
eval variable@(Var _) = variable
-- No rule for lambdas
eval lambda@(Lambda _ _) = lambda

eval (App e1 e2)
--
-- $ \frac{e_1 \to e_1'}{e_1 e_2 \to e_1' e_2} \quad (E-App1) $
--
 =
  let e1' = eval e1
--
-- $ \frac{e_2 \to e_2'}{v_1 e_2 \to v_1 e_2'} \quad (E-App2) $
--
   in let e2' = eval e2
       in case e1'
                of
--
-- $ {(\lambda x . e) v \to [x / v] e } \quad (E-AppLam) $
--
            (Lambda x e1'_body) -> eval $ substitute x e2' e1'_body
            e1' -> App e1' e2'

substitute :: Name -> Expr -> Expr -> Expr
--
-- If the Name matches: Substitute this Var by it's substitution
-- Otherwise: Leave it as is
--
substitute name substitution var@(Var varName)
  | name  == varName = substitution
  | otherwise = var
--
-- Recursively substitute in both parts of Applications
--
substitute name substitution (App term1 term2) =
  App (substitute name substitution term1) (substitute name substitution term2)

--
-- Only substitute in Lambda's body, if the parameter doesn't
-- redefine the Name in it's scope
--
substitute name substitution lambda@(Lambda varName term) =
  if name == varName
    then lambda
    else Lambda varName (substitute name substitution term)
