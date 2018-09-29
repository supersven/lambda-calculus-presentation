
module TypedCheck where

import Data.Either.Extra
import qualified Data.Map.Strict as Map

import TypedSyntax

check :: Environment -> Expr -> Either Name Type
--
-- $ \Gamma \vdash n : \text{Int}  \quad  \text{(T-Int)} $
--
check _ (IntValue _) = Right TInt
--
-- $ \Gamma \vdash \text{True} : \text{Bool}  \quad  \text{(T-True)} $
--
check _ (BoolValue True) = Right TBool
--
-- $ \Gamma \vdash \text{False} : \text{Bool}  \quad   \text{(T-False)} $
--
check _ (BoolValue False) = Right TBool

--
-- $ \frac{\Gamma, x : \tau_1 \vdash e : \tau_2}{\Gamma \vdash \lambda x:\tau_1 . e : \tau_1 \rightarrow \tau_2 }  \quad  \text{(T-Lam)} $
--
check env (Lambda name atype e) = do
  t <- check (Map.insert name atype env) e
  return $ TArr atype t
--
-- $  \frac{\Gamma \vdash e_1 : \tau_1 \rightarrow \tau_2 \quad \Gamma \vdash e_2 : \tau_1}{\Gamma \vdash e_1 e_2 : \tau_2}  \quad  \text{(T-App)} $
--
check env (App e1 e2) = do
  (TArr ta1 ta2) <- check env e1
  t2 <- check env e2
  if ta1 == t2
    then Right ta2
    else Left $ "Expected " ++ (show ta1) ++ " but got : " ++ (show t2)

--
-- $  \frac{x:\sigma \in \Gamma}{\Gamma \vdash x:\sigma}  \quad  \text{(T-Var)} $
--
check env (Var name) = find env name

find :: Environment -> Name -> Either Name Type
find env name = maybeToEither "Var not found!" (Map.lookup name env)
