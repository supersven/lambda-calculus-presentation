module TypedCheck where

import qualified Data.Map.Strict as Map
import Data.Either.Utils

type Name = String
type Environment = Map.Map Name Term

data Type  = TInt
           | TBool
           | TArr Type Type
           deriving (Eq, Show)

data Term = Variable Name |
              Application Term Term |
              Abstraction Name Type Term
              deriving (Eq, Show)

check :: Environment -> Term -> Either String Type
check env (Variable name) = maybeToEither "Variable not found!" (find env name)
check env (Application term1 term2) = TArr ()
check env (Abstract name atype term) =

find ::  Environment -> Name -> Maybe Type
find env name = Map.lookup name env
