module TypedCheck where

import qualified Data.Map.Strict as Map
import Data.Either.Extra

type Name = String
type Environment = Map.Map Name Type

data Type  = TInt
           | TBool
           | TArr Type Type
           deriving (Eq, Show)

data Term = Variable Name |
              Application Term Term |
              Abstraction Name Type Term
              deriving (Eq, Show)

check :: Environment -> Term -> Either String Type
check env (Variable name) = find env name
check env (Application term1 term2) =
  do
    (TArr ta1 ta2) <- check env term1
    t2 <- check env term2
    if ta1 == t2 then
      Right t2
    else
      Left $ "Expected " ++ (show ta1) ++ " but got : " ++ (show t2)
check env (Abstraction name atype term) = do
  t <- check (Map.insert name atype env) term
  return $ TArr atype t

find ::  Environment -> Name -> Either String Type
find env name = maybeToEither "Variable not found!" (Map.lookup name env)
