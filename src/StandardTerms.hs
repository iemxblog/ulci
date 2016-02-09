module StandardTerms (
    i
    , k
    , s
    , omega
    , y
) where

import Syntax

i :: LExpr
i = Lambda "x" (Var "x")

k :: LExpr
k = Lambda "x" (Lambda "y" (Var "x"))

s :: LExpr
s = Lambda "x" (Lambda "y" (Lambda "z" (Apply (Apply (Var "x") (Var "z")) (Apply (Var "y") (Var "z")))))

omega :: LExpr
omega = Lambda "x" (Apply (Var "x") (Var "x"))

y :: LExpr
y = Lambda "f" (Apply (Lambda "x" (Apply (Var "f") (Apply (Var "x") (Var "x")))) (Lambda "x" (Apply (Var "f") (Apply (Var "x") (Var "x")))))


