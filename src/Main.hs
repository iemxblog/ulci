module Main (
    main
) where

import System.IO
import Control.Monad
import Parser
import Syntax

interactPrompt :: String -> (String -> String) -> IO ()
interactPrompt p f = do
    putStr p
    hFlush stdout
    l <- getLine
    putStrLn $ f l
    interactPrompt p f

display :: Show b => Either b String -> String
display e = case e of
    Left err -> show err
    Right v -> v

main :: IO()
main = interactPrompt "ulci> " $ display . liftM pretty . parse
