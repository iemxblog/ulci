module Syntax (
    LExpr(..)
    , vars
    , pretty
) where

import Data.List

data LExpr
    = Var String
    | Lambda String LExpr
    | Apply LExpr LExpr
    deriving Eq


vars :: LExpr -> [String]
vars (Lambda v b) = v : vars b
vars _ = []

body :: LExpr -> LExpr
body (Lambda _ b) = body b
body e = e

instance Show LExpr where
    show = pretty

pretty' :: LExpr         -- ^ Expression to print
        -> Maybe LExpr  -- ^ Parent of the current expression
        -> String
pretty' (Var xs) _ = xs
pretty' l@(Lambda _ _) parent = case parent of
            Just (Apply _ _) -> "(" ++ showlambda ++ ")"
            _ -> showlambda
    where showlambda = "\x03bb" ++ (concat . intersperse " " . vars $ l) ++ "." ++ pretty' (body l) (Just l)
pretty' a@(Apply e1 e2) _ = pretty' e1 (Just a) ++ " " ++ case e2 of
    (Apply _ _) -> "(" ++ pretty' e2 (Just a) ++ ")"
    _ -> pretty' e2 (Just a)

pretty :: LExpr -> String
pretty = flip pretty' Nothing
