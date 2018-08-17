module UntypedEval where
import qualified Data.Map.Strict as Map

type Name = String
type Environment = Map.Map Name Term

data Term = Variable Name |
            Application Term Term |
            Abstraction Name Term
            deriving (Eq, Show)

eval ::Environment -> Term -> Maybe Term
eval env (Variable name) = find env name
eval env (Application term1 term2) = case term1 of
  (Abstraction name _) -> eval (Map.insert name term2 env) term1
  _ -> Nothing
eval _ abstraction@(Abstraction _ _) = Just abstraction

find ::  Environment -> Name -> Maybe Term
find env name = Map.lookup name env
