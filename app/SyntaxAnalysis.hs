-- | Performs type analysis on the syntax tree.
module SyntaxAnalysis where

import AlexUserState
import Control.Monad (zipWithM)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lexer
import ParserData

happyError :: Show s => s -> Alex a
happyError = strError . show

strError :: String -> Alex a
strError tok = do
  x <- getLineAndColumn
  alexError $ "Happy error on line and column " ++ show x ++ ": " ++ tok

defineData :: Name -> [Name -> Alex MultDef] -> Alex [ThreeAddressCode]
defineData name def = do
  s' <- alexGetUserState
  if any (Map.member name) $ s' ^. definitions
    then strError $ "Data name " ++ name ++ " is already defined at " ++ show (s' ^. definitions)
    else do
      alexSetUserState $ over definitions (\(x : xs) -> Map.insert name (DataDef name []) x : xs) s'
      def' <- traverse ($ name) def
      s <- alexGetUserState
      alexSetUserState $ over definitions (\(x : xs) -> Map.insert name (DataDef name def') x : xs) s
      concat <$> zipWithM generateMultDef def' [0 ..]

generateMultDef ::
  -- | Definition to convert to 3@C
  MultDef ->
  -- | We need a number to mark the type and discern between the sums.
  Int ->
  Alex [ThreeAddressCode]
generateMultDef (MultDef name params) x = do
  ref <- getRef
  let f (x, xs) dt = (sizeof dt + x, TacGetParam (RefInf ref x) : xs)
  return $
    [ TacFuncLabel name,
      TacIntCopy (RefInf ref 0) $ ConstantInt x
    ]
      ++ (reverse . (TacReturn (RefVar ref) :) . zipWith (flip ($)) [len -1, len -2 ..] . tail . snd $ foldl f (1, []) params)
  where
    len :: Int
    len = length params - 1 -- Minus return type

defineConstructor :: Name -> [Alex DataType] -> Name -> Alex MultDef
defineConstructor name xs' typeName = do
  s <- alexGetUserState
  xs <- sequenceA xs'
  b <- traverse nameIsDefined xs
  if and b
    then
      if any (Map.member name) $ s ^. values
        then strError $ "Constructor with name " ++ name ++ " is already defined at " ++ show (s ^. values)
        else do
          let dataTypes = reverse $ TypeDef typeName : xs
          -- Change Nothing to something more appropiate when val is defined.
          alexSetUserState $ over values (\(x : xs) -> Map.insert name (Value (TypeFun dataTypes) Nothing) x : xs) s
          return $ MultDef name dataTypes
    else
      let x = map snd . filter (not . fst) $ zip b xs
       in strError $ "Constructor with name " ++ name ++ " has used these unexistant types " ++ show x ++ "\n Relevant bindings: " ++ show s

nameIsDefined :: DataType -> Alex Bool
nameIsDefined (TypeDef name) = do
  s <- alexGetUserState
  return $ any (Map.member name) (s ^. definitions)
nameIsDefined _ = return True

defineTypeName :: Name -> Alex DataType
defineTypeName name =
  let def = TypeDef name
   in do
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
    over
      values
      -- TODO canviar el valor generat.
      (\(x : xs) -> Map.insert name (Value dataType Nothing) x : xs)
      s
  return def

type IntExp = (TacInt, [ThreeAddressCode])

getIntExp :: IntExp -> IntOp -> IntExp -> Alex IntExp
getIntExp (r, code) op (r', code') = do
  ref <- getRef <&> RefVar
  return (IntRef ref, code ++ code' ++ [TacIntOp ref r op r'])

getIntUnaryExp :: UnaryIntOp -> IntExp -> Alex IntExp
getIntUnaryExp op (r, code) = do
  ref <- getRef <&> RefVar
  return (IntRef ref, code ++ [TacIntUnary ref op r])

type RealExp = (TacReal, [ThreeAddressCode])

getRealExp :: RealExp -> RealOp -> RealExp -> Alex RealExp
getRealExp (r, code) op (r', code') = do
  ref <- getRef <&> RefVar
  return (RealRef ref, code ++ code' ++ [TacRealOp ref r op r'])

getRealUnaryExp :: UnaryRealOp -> RealExp -> Alex RealExp
getRealUnaryExp op (r, code) = do
  ref <- getRef <&> RefVar
  return (RealRef ref, code ++ [TacRealUnary ref op r])

type BoolExp = (TacBool, [ThreeAddressCode])

getBoolExp :: BoolExp -> BoolOp -> BoolExp -> Alex BoolExp
getBoolExp (r, code) op (r', code') = do
  ref <- getRef <&> RefVar
  return (BoolRef ref, code ++ code' ++ [TacBoolOp ref r op r'])

getBoolUnaryExp :: UnaryBoolOp -> BoolExp -> Alex BoolExp
getBoolUnaryExp op (r, code) = do
  ref <- getRef <&> RefVar
  return (BoolRef ref, code ++ [TacBoolUnary ref op r])

applyFunc :: Name -> [Alex Parameter] -> DataType -> Alex (Ref, [ThreeAddressCode])
applyFunc name params' dtype = do
  s <- alexGetUserState
  let vdicc = s ^. values
  params <- sequenceA params'
  if any (Map.member name) vdicc
    then do
      let val = (Map.! name) . head $ dropWhile (not . Map.member name) vdicc
      case correctApplication (val ^. dataType) (map getType params) of
        Left err -> strError err
        Right x ->
          if x == dtype
            then
              let initParams = concatMap getCode params
               in case x of
                    TypeFun xs -> return []
                    _ -> return (RefSp, reverse (TacCall name : map applyFunc' params))
            else strError $ "\n  Returned type is not the same as expected\n\t\t"
                 ++ show x ++ "\n\t\t"
                 ++ show dtype ++ "\n\tFunction: " ++ name ++ "\n\tParams: " ++ show params
    else strError $ "Name " ++ name ++ "is not defined. Relevant bindings: " ++ show (s ^. values)

applyFunc' :: Parameter -> ThreeAddressCode
applyFunc' p = case p of
  ParamInt ti _ -> TacPushParam $ RefInt ti
  ParamBool ti _ -> TacPushParam $ RefBool ti
  ParamReal ti _ -> TacPushParam $ RefReal ti

correctApplication :: DataType -> [DataType] -> Either String DataType
correctApplication (TypeFun xs) ys
  | length xs > length ys && and (zipWith (==) xs ys) = case drop (length ys) xs of
      [x] -> Right x
      xs -> Right $ TypeFun xs
  | otherwise = Left "Applyed more arguments than needed."
correctApplication _ (x : xs) = Left "Can this happen? I don't know"
correctApplication dt dts = Left "correctApplication not implemented"

defineIntExp :: IntExp -> DataType -> Alex (Ref, [ThreeAddressCode])
defineIntExp (a, ts) dto = return (RefInt a, ts)

defineRealExp :: RealExp -> DataType -> Alex (Ref, [ThreeAddressCode])
defineRealExp (a, ts) dto = return (RefReal a, ts)

defineBoolExp :: BoolExp -> DataType -> Alex (Ref, [ThreeAddressCode])
defineBoolExp (a, ts) dto = return (RefBool a, ts)

paramName :: Name -> Alex Parameter
paramName name = do
  s <- alexGetUserState
  let vdicc = s ^. values
  if any (Map.member name) vdicc
    then do
      let val = (Map.! name) . head $ dropWhile (not . Map.member name) vdicc
      -- Change Nothing to something more appropiate when val is defined.
      return $ ParamName name (val ^. dataType)
    else strError $ "Constructor with name " ++ name ++ " is already defined at " ++ show (s ^. values)

defineFunc :: Name -> [Alex DataType] -> (DataType -> Alex [ThreeAddressCode]) -> Alex [ThreeAddressCode]
defineFunc name [x] f = x >>= (TacFuncLabel name :) . f
defineFunc name dto f = sequenceA dto >>= f . TypeFun . reverse
