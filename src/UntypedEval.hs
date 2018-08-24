
module UntypedEval where
import qualified Data.Map.Strict as Map

type Name = String
type Environment = Map.Map Name Term

data Term = Variable Name |
            Application Term Term |
            Abstraction Name Term
            deriving (Eq, Show)

eval :: Environment -> Term -> Maybe Term
eval env (Variable name) = find env name
eval env (Application term1 term2) = case eval env term1 of
  Just (Abstraction name term) -> eval (Map.insert name term2 env) term
  Just term                    -> Just (Application term term2)
  Nothing -> Nothing
eval env abstraction@(Abstraction _ _) = Just abstraction

find ::  Environment -> Name -> Maybe Term
find env name = Map.lookup name env
