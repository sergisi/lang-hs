module Main where

import Parser
import ParserData
import Lexer

main :: IO ()
main =
  do
    s <- getContents
    print $ runAlex s calc
