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
  , getRef
  ) where

import AlexUserState
import Lens.Micro
}

%wrapper "monadUserState"

@names = [a-zA-Z][a-zA-Z0-9]*
@boolAnd = ("&&")|"∧"
@boolOr = ("||")|"∨"
@boolNot = ("!" )|"¬"
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
'.'             { token $ \(_, _, _, (_:y:_)) _ -> LChar y }
"=="            { tk LEq }
"!="            { tk LNeq }
"<"             { tk LLess }
"<="            { tk LLessEq }
">"             { tk LGreater }
">="            { tk LGreaterEq }
@boolAnd        { tk LBoolAnd }
@boolOr         { tk LBoolOr }
@boolNot        { tk LBoolNot }
">>"            { tk LRightShift }
"<<"            { tk LLeftShift }
\~              { tk LCompAUn }
\&              { tk LAnd }
\^              { tk LXor }
\{              { tk LOpenDef }
\}              { tk LCloseDef }
\[              { tk LOpenList }
\]              { tk LCloseList }
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
"data"          { tk LData }
"fun"           { tk LFun }
","             { tk LComma }
"()"            { tk LUnit }
"if"            { tk LConditional}
"else"          { tk LElse }
"while"         { tk LWhile }
"with"          { tk LWith }
"do"            { tk LDo }
<<<<<<< HEAD
"with"          { tk LWith }
=======
"for"           { tk LFor }
"map"           { tk LMap }
>>>>>>> 873216a67ca4434eb391b41e9410e775460ab270
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
            | LConditional
            | LElse
            | LOpenList
            | LCloseList
            | LWith
            | LFor
            | LMap
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
            | LData
            | LFun
            | LSumType
            | LComma
            | LEq
            | LNeq
            | LLess
            | LLessEq
            | LGreater
            | LGreaterEq
            | LBoolAnd
            | LBoolOr
            | LBoolNot
            | LUnit
            | LWhile
            | LDo
            | LWith
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

getRef :: Alex String
getRef = do
  s <- alexGetUserState
  alexSetUserState $ over tempRefs tail s
  return . head $ s ^. tempRefs

mainOther :: IO ()
mainOther = do
  s <- getContents
  print $ scanner s



}
