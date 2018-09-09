
module UntypedSyntaxExamples where

import UntypedSyntax

-- $true \equiv \lambda p . \ lambda q . p$
true :: Expr
true = Lambda "p" (Lambda "q" (Var "p"))

-- $false \equiv \lambda p . \ lambda q . q$
false :: Expr
false = Lambda "p" (Lambda "q" (Var "p"))

-- $if\_then\_else \equiv \lambda p . \lambda a . \lambda b . p a b $
if_then_else :: Expr
if_then_else =
  Lambda "p" (Lambda "a" (Lambda "b" (App (App (Var "p") (Var "a")) (Var "b"))))
