{
module Lexer
  ( LexerT(..)
  , scanner
  , Alex(..)
  , lexWrapper
  , alexError
  , runAlex
  , alexGetUserState
  , alexSetUserState
  , getLineAndColumn
  ) where

import AlexUserState
}

%wrapper "monadUserState"

@names = [a-zA-Z][a-zA-Z0-9]*

tokens :-

$white+    { skip }

\=              { tk LAssign }
\*              { tk LMult }
\/              { tk LDiv }
\+              { tk LSum }
\-              { tk LMinus }
\(              { tk LLBrack }
\)              { tk LRBrack }
"div"           { tk LDivInt }
"mod"           { tk LMod }
'.'             { token $ \(_, _, _, (x:y:xs)) len -> LChar y }
-- ">>"            { tk LRightShift }
-- "<<"            { tk LLeftShift }
-- \~              { tk LCompAUn }
-- \&              { tk LAnd }
-- \^              { tk LXor }
\{              { tk LOpenDef }
\}              { tk LCloseDef }
"true"          { tk $ LBool True }
"false"         { tk $ LBool False }
\;              { tk LSync }
"Real"          { tk LDefReal }
"Int"           { tk LDefInt }
"Bool"          { tk LDefBool }
"Char"          { tk LDefChar }
"->"            { tk LDefFunc }
"::"            { tk LTypeDef }
"case"          { tk LCase }
"return"        { tk LReturn }
"data"          { tk LData }
","             { tk LComma }
\|              { tk LSumType }
[0-9]+          { token (\(_, _, _, s) len -> LInt . read $ take len s) }
[0-9]+\.[0-9]+  { token (\(_, _, _, s) len -> LDouble . read $ take len s) }
@names          { token (\(_, _, _, s) len -> LVar $ take len s) }
{

tk :: LexerT -> AlexAction LexerT
tk = token . const . const

data LexerT = LMult
            | LDiv
            | LMod
            | LDivInt
            | LSum
            | LMinus
            | LLBrack
            | LRBrack
            | LInt Int
            | LVar String
            | LDouble Double
            | LBool Bool
            | LChar Char
            | LSync
            | LEOF
            | LAssign
            | LRightShift
            | LLeftShift
            | LCompAUn
            | LAnd
            | LOr
            | LXor
            | LDefReal
            | LDefInt
            | LDefBool
            | LDefFunc
            | LDefChar
            | LCase
            | LOpenDef
            | LCloseDef
            | LTypeDef
            | LReturn
            | LData
            | LSumType
            | LComma
            deriving (Show, Eq, Read, Ord)

scanner str = fmap reverse . runAlex str $ loop []

loop :: [LexerT] -> Alex [LexerT]
loop xs = do
  someToken <- alexMonadScan
  if someToken == LEOF
    then return $ xs
    else do
      loop $ someToken : xs


alexEOF :: Alex LexerT
alexEOF = return LEOF

lexWrapper :: (LexerT -> Alex a) -> Alex a
lexWrapper = (alexMonadScan >>=)


getLineAndColumn :: Alex (Int, Int)
getLineAndColumn = Alex $ \s@AlexState{alex_pos=pos} -> let (AlexPn _ line column) = pos in Right (s, (line, column))

mainOther :: IO ()
mainOther = do
  s <- getContents
  print $ scanner s

}
