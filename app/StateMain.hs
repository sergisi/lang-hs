
module Main where

import Parser

main :: IO ()
main =
  do
    s <- getContents
    case runExpression' s of
      Right x -> print x
      Left s' -> putStrLn s'
