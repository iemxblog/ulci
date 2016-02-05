module Main (
    main
) where

import Parser

main :: IO()
main = print $ parse "\\x.x x"
