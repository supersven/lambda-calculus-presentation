
module NaiveUntypedEval where
import qualified Data.Map.Strict as Map

type Name = String

data Term = Variable Name |
            Application Term Term |
            Abstraction Name Term
            deriving (Eq, Show)

eval :: Term -> Term
eval (Variable name) = Variable name
eval (Application term1 term2) = case eval term1 of
  (Abstraction name term1') -> eval $ substitute name term2 term1'
  term                    -> Application term term2
eval abstraction@(Abstraction _ _) = abstraction

substitute :: String -> Term -> Term -> Term
substitute name substitution (Variable varName) = if name == varName then
                                                    substitution
                                                  else
                                                    Variable varName
substitute name substitution (Application term1 term2) = Application (substitute name substitution term1)  (substitute name substitution term2)
substitute name substitution (Abstraction varName term) = if name == varName then
                                                            Abstraction varName term
                                                          else
                                                            Abstraction varName (substitute name substitution term)
