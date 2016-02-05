module Main (
    main
) where

import System.IO
import Parser

interactPrompt :: String -> (String -> String) -> IO ()
interactPrompt p f = do
    putStr p
    hFlush stdout
    l <- getLine
    putStrLn $ f l
    interactPrompt p f

main :: IO()
main = interactPrompt "ulci> " $ show . parse
