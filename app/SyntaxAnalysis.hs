-- | Performs type analysis on the syntax tree.

module SyntaxAnalysis where

import ParserData
import Lexer
import AlexUserState
import qualified Data.Map.Strict as Map

import Lens.Micro

happyError :: Show s => s -> Alex a
happyError = strError . show

strError :: String -> Alex a
strError tok = do
  x <- getLineAndColumn
  alexError $ "Happy error on line and column " ++ show x ++ ": " ++ tok

defineData :: Name -> [Name -> Alex MultDef] -> Alex Exp
defineData name def = do
  s' <- alexGetUserState
  if any (Map.member name)  $ s' ^. definitions
    then strError $ "Data name " ++ name ++ " is already defined at " ++ show (s' ^. definitions)
    else do
      alexSetUserState $ over definitions (\(x:xs) -> Map.insert name (DataDef name []) x : xs) s'
      def' <- traverse ($ name) def
      s <- alexGetUserState
      alexSetUserState $ over definitions (\(x:xs) -> Map.insert name (DataDef name def') x : xs) s
      return TNone

defineConstructor :: Name -> [Alex DataType] -> Name -> Alex MultDef
defineConstructor name xs' typeName = do
  s <- alexGetUserState
  xs <- sequenceA xs'
  b <- traverse nameIsDefined xs
  if and b then
    if any (Map.member name)  $ s ^. values
      then strError $ "Constructor with name " ++ name ++ " is already defined at " ++ show (s ^. values)
      else do
        let dataTypes = reverse $ TypeDef typeName : xs
        -- Change Nothing to something more appropiate when val is defined.
        alexSetUserState $ over values (\(x:xs) -> Map.insert name (Value (TypeFun dataTypes) Nothing) x : xs) s
        return $ MultDef name dataTypes
    else
      let x = map snd . filter (not . fst) $ zip b xs in
      strError $ "Constructor with name " ++ name ++ " has used these unexistant types " ++ show x ++ "\n Relevant bindings: " ++ show s

nameIsDefined :: DataType -> Alex Bool
nameIsDefined (TypeDef name) = do
  s <- alexGetUserState
  return $ any (Map.member name) (s ^. definitions)
nameIsDefined _ = return True

defineTypeName :: Name -> Alex DataType
defineTypeName name = let def = TypeDef name in do
  b <- nameIsDefined def
  s <- alexGetUserState
  if b
    then return def
    else strError $ "name " ++ name ++ " is not defined at " ++ show (s ^. definitions)

defineValue :: Name -> Alex DataType -> Exp -> Alex Exp
defineValue name dtype' def = do
  dataType <- dtype'
  s <- alexGetUserState
  alexSetUserState $
    over values
    -- TODO canviar el valor generat.
    (\(x:xs) -> Map.insert name (Value dataType Nothing) x : xs)
    s
  return def
