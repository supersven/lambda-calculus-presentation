
module UntypedSyntaxExamples where

import UntypedSyntax

-- $true \equiv \lambda p . \lambda q . p$
true :: Expr
true = Lambda "p" (Lambda "q" (Var "p"))

-- $false \equiv \lambda p . \lambda q . q$
false :: Expr
false = Lambda "p" (Lambda "q" (Var "q"))

-- $and \equiv \lambda p . \lambda q . p \ q \ p$
and :: Expr
and = Lambda "p" $ Lambda "q" $ App (App (Var "p") (Var "q")) (Var "p")
