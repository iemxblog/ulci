module Parser
(
    parse
) where

import Text.Parsec hiding (parse)
import qualified Text.Parsec
import Control.Monad
import Syntax


curryfy :: [String] -> LExpr -> LExpr
curryfy [] b = b
curryfy (x:xs) b = Lambda x (curryfy xs b)

tok :: Parsec String () a -> Parsec String () a
tok p = do
    spaces
    r <- p
    spaces
    return r

parens :: Parsec String () a -> Parsec String () a
parens p = do
    tok $ char '('
    r <- p
    tok $ char ')'
    return r

name :: Parsec String () String
name = tok $ do
    cs <- many1 letter
    ds <- many digit
    return $ cs ++ ds

var :: Parsec String () LExpr
var = liftM Var name

lambda :: Parsec String () LExpr
lambda = tok $ do
    char '\\'
    ns <- many1 name
    char '.'
    e <- lexpr
    return $ curryfy ns e

term :: Parsec String () LExpr
term = (var <|> lambda <|> parens lexpr)

lexpr :: Parsec String () LExpr
lexpr = do
    ts <- many1 term
    return $ foldl1 Apply ts

parse = Text.Parsec.parse lexpr "ulci" 
