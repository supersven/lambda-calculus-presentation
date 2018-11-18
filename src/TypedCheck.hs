
module TypedCheck where

import Data.Either.Extra
import qualified Data.Map.Strict as Map

import TypedSyntax

check :: Environment -> Expr -> Either Error Type
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
check env (Lambda x t1 e) = do
  t2 <- check (Map.insert x t1 env) e
  return $ TArr t1 t2

--
-- $  \frac{\Gamma \vdash e_1 : \tau_1 \rightarrow \tau_2 \quad \Gamma \vdash e_2 : \tau_1}{\Gamma \vdash e_1 e_2 : \tau_2}  \quad  \text{(T-App)} $
--
check env (App e1 e2) = do
  te1 <- check env e1
  case te1 of
    (TArr t1 t2) -> do
      te2 <- check env e2
      if t1 == te2
        then Right t2
        else Left $ "Expected " ++ (show t1) ++ " but got : " ++ (show te2)
    _ -> Left $ "Expected TArr but got : " ++ (show te1)

--
-- $  \frac{x:\tau \in \Gamma}{\Gamma \vdash x:\tau}  \quad  \text{(T-Var)} $
--
check env (Var x) = find env x

find :: Environment -> Name -> Either Name Type
find env name = maybeToEither "Var not found!" (Map.lookup name env)
