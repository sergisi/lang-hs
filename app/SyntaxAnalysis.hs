{-# LANGUAGE LambdaCase #-}

-- | Performs type analysis on the syntax tree.
module SyntaxAnalysis where

import AlexUserState
import Control.Monad (zipWithM)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lexer
import ParserData

type Code = [ThreeAddressCode]

type RefCodeDt = (Ref, Code, DataType)

type Exp' = Either (DataType -> Alex RefCodeDt) RefCodeDt

type Exp = Alex Exp'

-- * Utility Functions

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

trd' :: (a, b, c) -> c
trd' (_, _, c) = c

checkType :: DataType -> Exp' -> Alex RefCodeDt
checkType dtype (Right a@(_, _, dtype'))
  | dtype == dtype' = return a
  | otherwise =
    strError $
      "Type Error: "
        ++ "\n\t\tExpected: "
        ++ repr dtype
        ++ "\n\t\t     Got: "
        ++ repr dtype'
checkType dtype (Left f) = f dtype >>= checkType dtype . Right

happyError :: Show s => s -> Alex a
happyError = strError . show

strError :: String -> Alex a
strError tok = do
  x <- getLineAndColumn
  alexError $ "Happy error on line and column " ++ show x ++ ": " ++ tok

-- | Wraps function definition
wrapFunction :: [Name] -> Name -> Name -> [ThreeAddressCode] -> Ref -> DataType -> RefCodeDt
wrapFunction params funcName later code ref dt =
  ( RefFunc funcName,
    TacGoto later :
    TacFuncLabel funcName :
    zipWith TacGetParam (map RefVar params) [1 ..]
      ++ code
      ++ [ TacReturn ref,
           TacLabel later
         ],
    dt
  )

-- | Adds a new Context
newContext :: Alex AlexUserState
newContext = do
  s <- alexGetUserState
  let s' =
        over values (Map.empty :) $
          over
            definitions
            (Map.empty :)
            s
  alexSetUserState s'
  return s'

-- | Removes the last context if it does not leave [] as the actual context.
removeContext :: Alex AlexUserState
removeContext = do
  s <- alexGetUserState >>= removeContext'
  alexSetUserState s
  return s

removeContext' :: AlexUserState -> Alex AlexUserState
removeContext' a@(AlexUserState vs ds _)
  | length vs == 1 = strError "Invalid State: Removed values context when there was only one left."
  | length ds == 1 = strError "Invalid State: Removed definitions context when there was only one left."
  | otherwise = return . over values tail $ over definitions tail a

-- * Parser.y functions

getNameParam :: Name -> Exp
getNameParam name = do
  s <- alexGetUserState
  let vdicc = s ^. values
  if any (Map.member name) vdicc
    then
      let val = (Map.! name) . head $ dropWhile (not . Map.member name) vdicc
       in return $ Right (RefVar name, [], val)
    else strError $ "Name " ++ name ++ " is not defined."

defineData :: Name -> [Name -> Alex MultDef] -> Alex Code
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
  Alex Code
generateMultDef (MultDef name params) x = do
  ref <- getRef
  let f (y, ys) dt = (sizeof dt + y, TacGetParam (RefInf ref y) : ys)
  return $
    [ TacFuncLabel name,
      TacCopy (RefInf ref 0) $ RefConstInt x
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
          s' <- alexGetUserState
          alexSetUserState $ over values (\(z : zs) -> Map.insert name (TypeFun dataTypes) z : zs) s'
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
defineTypeName name = do
  let def = TypeDef name
  b <- nameIsDefined def
  s <- alexGetUserState
  if b
    then return def
    else strError $ "name " ++ name ++ " is not defined at " ++ show (s ^. definitions)

getExp :: DataType -> Exp -> Op -> Exp -> Exp
getExp dt e op e' = do
  ref <- getRef <&> RefVar
  (r, code, _) <- e >>= checkType dt
  (r', code', _) <- e' >>= checkType dt
  return $ Right (ref, code ++ code' ++ [TacOp ref r op r'], dt)

-- | Gets integer / real / bool / char expression
-- Returns another type, as in == operation between integers
getExp' :: DataType -> DataType -> Exp -> Op -> Exp -> Exp
getExp' dtRes dtDefs e op e' = do
  ref <- getRef <&> RefVar
  (r, code, _) <- e >>= checkType dtDefs
  (r', code', _) <- e' >>= checkType dtDefs
  return $ Right (ref, code ++ code' ++ [TacOp ref r op r'], dtRes)

getUnaryExp :: DataType -> UnaryOp -> Exp -> Exp
getUnaryExp dt op e' = do
  ref <- getRef <&> RefVar
  (r', code', _) <- e' >>= checkType dt
  return $ Right (ref, code' ++ [TacUnary ref op r'], dt)

moreParamsThanAppliable :: [a] -> [b] -> Alex ()
moreParamsThanAppliable xs params
  | length xs <= length params =
    strError $
      "Applied more arguments than needed:"
        ++ "\n\t\tApplied: "
        ++ show (length params)
        ++ "\n\t\t    Got: "
        ++ show (length xs)
  | otherwise = return ()

noParameters :: (Repr b) => [a] -> b -> Alex ()
noParameters params x
  | not (null params) =
    strError
      ( "Applied Function to value: "
          ++ "\n\t\t               Value: "
          ++ repr x
          ++ "\n\t\tNumber of parameters: "
          ++ show (length params)
      )
  | otherwise = return ()

curryficateFunction :: Name -> [RefCodeDt] -> DataType -> Alex RefCodeDt
curryficateFunction name appliedParams dt = do
  funcName <- getRef
  later <- getRef
  -- Idea:
  --   goto later; Func funcName [Def ...] label later;
  return
      ( RefFunc funcName,
        TacGoto later :
        TacFuncLabel funcName :
        concatMap snd' appliedParams
          ++ map (TacPushParam . fst') appliedParams
          ++ [ TacCall name,
               TacReturn RefSP,
               TacLabel later
             ],
        dt
      )

ifMemberError :: [Map.Map String v] -> String -> Alex ()
ifMemberError vdicc name
  | any (Map.member name) vdicc = return ()
  | otherwise = strError $ "Apply Function name " ++ name ++ " is not defined"

returnVar :: Name -> [RefCodeDt] -> DataType -> Alex RefCodeDt
returnVar name appliedParams dt = do
  r <- getRef <&> RefVar
  return
      ( r,
        reverse
          ( TacCopy r RefSP :
            TacCall name :
            map (TacPushParam . fst') appliedParams
              ++ concatMap snd' appliedParams
          ),
        dt
      )


applyFunc :: Name -> [Exp] -> Exp
applyFunc name params = do
  s <- alexGetUserState
  let vdicc = s ^. values
  ifMemberError vdicc name
  let val = (Map.! name) . head $ dropWhile (not . Map.member name) vdicc
  case val of
    TypeFun xs ->
      do
        moreParamsThanAppliable xs params
        appliedParams <- zipWithM (\x y -> y >>= checkType x) xs params
        if length xs - 1 == length params
          then Right <$> returnVar name appliedParams (last xs)
          else Right <$> curryficateFunction name appliedParams (TypeFun $ drop (length params) xs)
    x -> do
      noParameters params x
      return $ Right (RefVar name, [], x)

-- | Define Func only for Outer Assigns
defineFunc ::
  -- |  Name of the function
  Name ->
  -- |  Types of fun
  [Alex DataType] ->
  -- |  Def
  Exp ->
  Alex [ThreeAddressCode]
defineFunc name [x'] _ = do
  x <- x'
  strError $ "Can not have global constants, please declare using unit: " ++ name ++ ":: () -> " ++ repr x ++ " = definition"
defineFunc name dto exp = do
  x' <- sequenceA dto
  let x'' = TypeFun $ reverse x'
  s <- alexGetUserState
  alexSetUserState $ over values (\(y : ys) -> Map.insert name x'' y : ys) s
  (ref, code, _) <- exp >>= checkType x''
  case ref of
    RefFunc r ->
      return $
        TacFuncLabel name :
        code
          ++ [ TacCall r,
               TacReturn RefSP
             ]
    _ -> return $ TacFuncLabel name : code ++ [TacReturn ref]

-- | Define Func only for Inner assigns (statements)
defineFunc' ::
  -- | Name of the function
  Name ->
  -- | Types of fun
  [Alex DataType] ->
  -- | Def
  Exp ->
  Alex [ThreeAddressCode]
defineFunc' name [x] exp = do
  x' <- x
  s <- alexGetUserState
  alexSetUserState $ over values (\(y : ys) -> Map.insert name x' y : ys) s
  (ref, code, _) <- exp >>= checkType x'
  return $ code ++ [TacCopy (RefVar name) ref]
defineFunc' a b c = defineFunc a b c

-- TODO how do I change this?

functionDef ::
  -- | Names
  [Name] ->
  -- | Statements
  Alex Code ->
  -- | Def
  Exp ->
  -- | Non inferable type.
  DataType ->
  Alex (Ref, Code, DataType)
functionDef names mcode def dt@(TypeFun xs)
  | length xs <= length names =
    strError $ "Defined more parameters than the function has: " ++ repr (TypeFun xs) ++ "; with parameters: " ++ unwords names
  | otherwise =
    do
      funcName <- getRef
      later <- getRef
      s <- newContext
      let s'' = over values (\(_ : vs) -> Map.fromList (zip names xs) : vs) s
      alexSetUserState s''
      code <- mcode
      let xs' = drop (length names) xs
      (ref, code', dtype) <- def >>= checkType (if length xs' == 1 then head xs' else TypeFun xs')
      _ <- removeContext
      return $ wrapFunction names funcName later (code ++ code') ref dt
functionDef [] mcode def x =
  do
    _ <- newContext
    funcName <- getRef
    later <- getRef
    code <- mcode
    (ref, code', _) <- def >>= checkType x
    _ <- removeContext
    return $ wrapFunction [] funcName later (code ++ code') ref x
functionDef _ _ _ _ = strError "Invalid definition of a function."

-- while :: (acc -> Bool) -> (acc -> acc) -> acc -> acc
-- if Bool a a

type Statements = Alex [ThreeAddressCode]

defineConditional :: Exp -> Statements -> Exp -> Statements -> Exp -> Exp
defineConditional boolExp stsThen expThen stsElse expElse = do
  boolExp' <- boolExp >>= checkType TypeBool
  _ <- newContext
  codeThen <- stsThen
  expThen' <- expThen
  _ <- removeContext
  _ <- newContext
  codeElse <- stsElse
  expElse' <- expElse
  _ <- removeContext
  case canInferCond expThen' expElse' of
    Just x -> Right <$> defCond boolExp' codeThen expThen' codeElse expElse' x
    Nothing -> return . Left $ defCond boolExp' codeThen expThen' codeElse expElse'

canInferCond :: Exp' -> Exp' -> Maybe DataType
canInferCond (Right (_, _, x)) _ = Just x
canInferCond _ (Right (_, _, x)) = Just x
canInferCond _ _ = Nothing

defCond :: RefCodeDt -> Code -> Exp' -> Code -> Exp' -> DataType -> Alex RefCodeDt
defCond (refBool, boolCode, _) codeThen expThen codeElse expElse dtype = do
  (refThen, codeThen', _) <- checkType dtype expThen
  (refElse, codeElse', _) <- checkType dtype expElse
  labelElse <- getRef
  res <- getRef <&> RefVar
  labelFinal <- getRef
  return
    ( res,
      boolCode
        ++ TacIfExp refBool labelElse :
      codeThen ++ codeThen'
        ++ TacCopy res refThen :
      TacGoto labelFinal :
      TacLabel labelElse :
      codeElse ++ codeElse'
        ++ [ TacCopy res refElse,
             TacLabel labelFinal
           ]
    , dtype
    )

-- | map Def
mapDef ::
  -- | Array [a]
  Exp ->
  -- | a -> b
  Exp ->
  -- | [b]
  Exp
mapDef axs ado = undefined

-- | for Def
forDef ::
  -- | Array [a]
  Exp ->
  -- | acc
  Exp ->
  -- | acc -> a -> acc
  Exp ->
  Exp
forDef axs ainit ado = 
  do
    axs' <- axs
    ainit' <- ainit
    ado' <- ado
    forDef' axs' ainit' ado'


forDef' :: 
-- | Array [a]
  Exp' ->
  -- | acc
  Exp' ->
  -- | acc -> a -> acc
  Exp' ->
  Exp
forDef' (Right (refArr, codeArr, typeArr)) ainit' ado' = 
  case typeArr of
    TypeArray a -> 
      case ainit' of
        Left f -> _ 
        Right (refAinit, codeAinit, ainitType) ->
          do
          (refAdo, codeAdo, _) <- checkType (TypeFun [ainitType ,typeArr, ainitType])
    
forDef'' :: DataType -> DataType -> Exp' -> Exp' -> Exp'

maybeHead :: [a] -> Maybe a
maybeHead (x : xs) = Just x
maybeHead [] = Nothing

getInferedType :: [Exp'] -> Maybe DataType
getInferedType =
  fmap (\(Right (_, _, x)) -> x)
    . maybeHead
    . dropWhile
      ( \case
          Left _ -> True
          Right _ -> False
      )

defineArray' :: [Exp'] -> DataType -> Alex RefCodeDt
defineArray' xs x = do
  xs' <- traverse (checkType x) xs
  ref <- getRef
  let refSup = RefInf ref
  return
    ( RefVar ref,
      concatMap snd' xs'
        ++ TacCopy (refSup 0) (RefConstInt (length xs)) :
      zipWith
        (TacCopy . refSup)
        [1, 1 + sizeof x ..]
        (reverse $ map fst' xs'),
      TypeArray x
    )

defineArray :: [Exp] -> Exp
defineArray xs = do
  xs' <- sequenceA xs
  case getInferedType xs' of
    Just x -> Right <$> defineArray' xs' x
    Nothing -> return . Left $ defineArray' xs'

-- | While definition
whileDef ::
  -- | Bool definition (acc -> Bool)
  Exp ->
  -- | with definition aka initialization bloc (acc)
  Exp ->
  -- | code definition (acc -> acc)
  Exp ->
  DataType ->
  Alex (Ref, Code, DataType)
whileDef conditionFunc iniAcc accFunc dtype =
  do
    (refCond, codeCond, _) <- conditionFunc >>= checkType (TypeFun [dtype, TypeBool])
    nameCond <- getStringOfRef refCond
    (refIniAcc, codeIniAcc, _) <- iniAcc >>= checkType dtype
    (refFuncAcc, codeAcc, _) <- accFunc >>= checkType (TypeFun [dtype, dtype])
    nameFuncAcc <- getStringOfRef refFuncAcc
    refAcc <- getRef <&> RefVar
    bucle <- getRef
    out <- getRef
    return
      ( refAcc,
        codeIniAcc
          ++ codeCond
          ++ codeAcc
          ++ [ TacCopy refAcc refIniAcc,
               TacLabel bucle,
               TacPushParam refAcc,
               TacCall nameCond,
               TacIfExp RefSP out,
               TacPushParam refAcc,
               TacCall nameFuncAcc,
               TacCopy refAcc RefSP,
               TacGoto bucle,
               TacLabel out
             ],
        dtype
      )
-- | repeat/until definition
repeatUntilDef ::
  -- | code definition (acc -> acc)
  Exp ->
  -- | Bool definition (acc -> Bool)
  Exp ->
  -- | with definition aka initialization bloc (acc)
  Exp ->
  DataType ->
  Alex (Ref, Code, DataType)
repeatUntilDef accFunc conditionFunc iniAcc dtype =
  do
    (refCond, codeCond, _) <- conditionFunc >>= checkType (TypeFun [dtype, TypeBool])
    nameCond <- getStringOfRef refCond
    (refIniAcc, codeIniAcc, _) <- iniAcc >>= checkType dtype
    (refFuncAcc, codeAcc, _) <- accFunc >>= checkType (TypeFun [dtype, dtype])
    nameFuncAcc <- getStringOfRef refFuncAcc
    refAcc <- getRef <&> RefVar
    bucle <- getRef
    return
      ( refAcc,
        codeIniAcc
          ++ codeCond
          ++ codeAcc
          ++ [ TacCopy refAcc refIniAcc,
               TacLabel bucle,
               TacPushParam refAcc,
               TacPushParam refAcc,
               TacCall nameFuncAcc,
               TacCopy refAcc RefSP,
               TacCall nameCond,
               TacIfExp RefSP bucle
             ],
        dtype
      )

getStringOfRef :: Ref -> Alex String
getStringOfRef ref = case ref of
  RefVar n -> return n
  RefFunc n -> return n
  _ -> strError "Expected: Function, Got: Constant"
