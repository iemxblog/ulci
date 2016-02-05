module Syntax (
    
    LExpr(..)
) where

data LExpr
    = Var String
    | Lambda String LExpr
    | Apply LExpr LExpr
    deriving (Eq, Show)
