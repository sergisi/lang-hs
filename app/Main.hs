module Main where

import Parser
import ParserData
import Lexer

main :: IO ()
main =
  do
    s <- getContents
    case runExpression s of
      Right x -> print x
      Left s -> putStrLn s
