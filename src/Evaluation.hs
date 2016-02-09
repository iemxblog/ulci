module Evaluation (
    fv
    , subst
    , fresh
    , evalHistory
    , eval
) where

import Data.Char
import Syntax

-- | Returns the free variables of an expression.
fv :: LExpr -> [String]
fv (Var x) = [x]
fv (Lambda x b) = filter (/=x) $ fv b
fv (Apply e1 e2) = fv e1 ++ fv e2

-- | Capture avoiding substitution.
subst :: LExpr       -- ^ Expression in which we do the substitution
                -> String   -- ^ Name of the variable to substitue
                -> LExpr    -- ^ Content of the substitution
                -> LExpr    -- ^ Resulting expression
subst (Var x) y n | x == y = n
subst (Var y) x _ | x/= y = Var y
subst (Apply e1 e2) x n = Apply (subst e1 x n) (subst e2 x n)
subst (Lambda x m) y _ | x == y = Lambda x m
subst (Lambda y m) x n | x /= y && not (y `elem` fv n) = Lambda y (subst m x n)
subst (Lambda y m) x n | x /= y && y `elem` fv n = Lambda z (subst (subst m y (Var z)) x n) 
    where z = fresh y ( x : (fv m ++ fv n))

-- | Generates a fresh variable.
fresh :: String         -- ^ Initial name of the variable
        -> [String]     -- ^ Set of forbidden variable names
        -> String
fresh v xs = head . filter (not . (`elem` (v:xs))) $ [va ++ show i | i <- [1..]]
    where va = filter (isAlpha) v


-- | Evaluates an expression, but makes a single step.
evalStep :: LExpr       -- ^ Expression to evaluate 
            -> LExpr    -- ^ Partially evaluated expression.
evalStep (Apply (Lambda x e1) e2) = subst e1 x e2
evalStep (Var x) = (Var x)
evalStep (Apply e1 e2) = Apply (evalStep e1) (evalStep e2)
evalStep (Lambda x e) = Lambda x (evalStep e)

-- | Checks if the evaluation has ended (no more reductions to make).
end :: LExpr -> Bool
end (Apply (Lambda _ _) _) = False
end (Var _) = True
end (Apply e1 e2) = end e1 && end e2
end (Lambda _ e) = end e

-- | Generates printable evaluation history, showing reduction steps.
evalHistory :: LExpr -> String
evalHistory e = show e ++ "\n" ++ next
    where next = case end e of
            True -> ""
            False -> "=> " ++ evalHistory (evalStep e)

-- | Returns the result of the evaluation of an expression.
eval :: LExpr -> LExpr
eval e = case end e of
            True -> e
            False -> eval (evalStep e)
