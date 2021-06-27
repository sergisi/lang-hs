{-# LANGUAGE LambdaCase #-}

-- | Performs type analysis on the syntax tree.
module SyntaxAnalysis where

import AlexUserState
import Control.Applicative
import Control.Monad (zipWithM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro
import Lexer
import ParserData

type Code = [ThreeAddressCode]

type RefCodeDt = (Ref, Code, DataType)

type Exp' = Either (DataType -> Alex RefCodeDt) RefCodeDt

type Exp = Alex Exp'

type CaseAcc = (Map.Map Name ([DataType], Int), Set.Set Name, Map.Map Int (Label, Code))

-- * Utility Functions

fst' :: (a, b, c) -> a
fst' (a, _, _) = a

snd' :: (a, b, c) -> b
snd' (_, b, _) = b

trd' :: (a, b, c) -> c
trd' (_, _, c) = c

checkType' :: DataType -> Exp' -> Alex (Maybe RefCodeDt)
checkType' dtype (Right a@(_, _, dtype'))
  | dtype == dtype' = return $ Just a
  | otherwise = return Nothing
checkType' dtype (Left f) = f dtype >>= checkType' dtype . Right

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
  ref <- getRef <&> RefVar
  let f (y, ys) dt = (sizeof dt + y, TacGetParam (RefInf ref $ RefConstInt y) : ys)
  return $
    [ TacFuncLabel name,
      TacCopy (RefInf ref $ RefConstInt 0) $ RefConstInt x
    ]
      ++ (reverse . (TacReturn ref :) . zipWith (flip ($)) [len -1, len -2 ..] . tail . snd $ foldl f (1, []) params)
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

defineExp :: [DataType] -> Exp -> Op -> Exp -> Exp
defineExp dts e op e' = do
  ref <- getRef <&> RefVar
  mayDef <- traverse (getExp e e') dts <&> foldl (<|>) Nothing
  case mayDef of
    Just ((r, code, dt), (r', code', _)) -> return $ Right (ref, code ++ code' ++ [TacOp ref r op r'], dt)
    Nothing -> strError $ "None of the types: " ++ show (map repr dts) ++ " are apliable"

getExp :: Exp -> Exp -> DataType -> Alex (Maybe (RefCodeDt, RefCodeDt))
getExp e e' dt = do
  mayExp <- e >>= checkType' dt
  mayExp' <- e' >>= checkType' dt
  return $ (,) <$> mayExp <*> mayExp'

defineExp' :: DataType -> [DataType] -> Exp -> Op -> Exp -> Exp
defineExp' dtRes dts e op e' = do
  ref <- getRef <&> RefVar
  mayDef <- traverse (getExp e e') dts <&> foldl (<|>) Nothing
  case mayDef of
    Just ((r, code, _), (r', code', _)) -> return $ Right (ref, code ++ code' ++ [TacOp ref r op r'], dtRes)
    Nothing -> strError $ "None of the types: " ++ show (map repr dts) ++ " are apliable"

defineUnaryExp :: [DataType] -> UnaryOp -> Exp -> Exp
defineUnaryExp dts op e' = do
  ref <- getRef <&> RefVar
  mayDef <- traverse (getUnaryExp e') dts <&> foldl (<|>) Nothing
  case mayDef of
    Just (r', code', dt) -> return $ Right (ref, code' ++ [TacUnary ref op r'], dt)
    Nothing -> strError $ "None of the types: " ++ show (map repr dts) ++ " are apliable"

getUnaryExp :: Exp -> DataType -> Alex (Maybe RefCodeDt)
getUnaryExp e dt = e >>= checkType' dt

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
        ++ [ TacCall $ RefVar name,
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
          TacCall (RefVar name) :
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
defineFunc name dto e = do
  x' <- sequenceA dto
  let x'' = TypeFun $ reverse x'
  s <- alexGetUserState
  alexSetUserState $ over values (\(y : ys) -> Map.insert name x'' y : ys) s
  (ref, code, _) <- e >>= checkType x''
  case ref of
    RefFunc r ->
      return $
        TacFuncLabel name :
        code
          ++ [ TacCall $ RefVar r,
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
defineFunc' name [x] e = do
  x' <- x
  s <- alexGetUserState
  alexSetUserState $ over values (\(y : ys) -> Map.insert name x' y : ys) s
  (ref, code, _) <- e >>= checkType x'
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
      (ref, code', _) <- def >>= checkType (if length xs' == 1 then head xs' else TypeFun xs')
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
           ],
      dtype
    )

-- * Map Def

-- | map Def
mapDef ::
  -- | Array [a]
  Exp ->
  -- | a -> b
  Exp ->
  -- | [b]
  Exp
mapDef axs ado =
  do
    axs' <- axs
    ado' <- ado
    mapDef' axs' ado'

mapDef' ::
  -- | [a]
  Exp' ->
  -- | a -> b
  Exp' ->
  -- | [b]
  Exp
mapDef' axs ado@(Right (_, _, typeDo)) =
  case typeDo of
    TypeFun [a, b] ->
      Right <$> mapDef'' axs ado a (TypeArray b)
    _ -> strError $ "Do block must be a function, but got: " ++ repr typeDo
mapDef' axs'@(Right (_, _, typeArr)) ado' =
  case typeArr of
    TypeArray a -> return . Left $ mapDef'' axs' ado' a
    x -> strError $ "Expected any kind of list, but got: " ++ repr x
mapDef' _ _ = strError "Couldn't infer type of array nor function. Introduce a constant so it can infer the list."

mapDef'' ::
  -- | [a]
  Exp' ->
  -- | a -> b
  Exp' ->
  -- | a
  DataType ->
  -- | b
  DataType ->
  Alex RefCodeDt
mapDef'' axs ado a (TypeArray b) = do
  (refArr, codeArr, _) <- checkType (TypeArray a) axs
  (refAdo, codeAdo, _) <- checkType (TypeFun [a, b]) ado
  ref <- getRef <&> RefVar
  refIt <- getRef <&> RefVar
  refMaxIt <- getRef <&> RefVar
  bucle <- getRef
  endFor <- getRef
  boolExp <- getRef <&> RefVar
  return
    ( ref,
      codeArr ++ codeAdo
        ++ TacCopy (RefInf ref $ RefConstInt 0) (RefInf refArr $ RefConstInt 0) :
      TacCopy refIt (RefConstInt 1) :
      TacOp refMaxIt (RefInf refArr (RefConstInt 0)) OpMult (RefConstInt $ sizeof a) :
      TacOp refMaxIt refMaxIt OpSum (RefConstInt 1) :
      TacLabel bucle :
      TacOp boolExp refIt OpLt refMaxIt :
      TacIfExp boolExp endFor :
      TacPushParam (RefInf refArr refIt) :
      TacCall refAdo :
      TacCopy (RefInf ref refIt) RefSP :
      TacOp refIt refIt OpSum (RefConstInt (sizeof a)) :
      TacGoto bucle :
      [TacLabel endFor],
      TypeArray b
    )
mapDef'' _ _ _ x = strError $ "Map always return an array, but expected: " ++ repr x

-- * Real for def

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
forDef' axs ainit ado@(Right (_, _, typeDo)) =
  case typeDo of
    TypeFun [acc, a, acc'] ->
      if acc == acc'
        then Right <$> forDef'' axs ainit ado a acc
        else
          strError $
            "TypeError: "
              ++ "\n\t\tExpected: "
              ++ repr (TypeFun [acc, a, acc])
              ++ "\n\t\t      Or: "
              ++ repr (TypeFun [acc', a, acc'])
              ++ "\n\t\t     Got: "
              ++ repr (TypeFun [acc, a, acc'])
    _ -> strError $ "Do block must be a function, but got: " ++ repr typeDo
forDef' axs'@(Right (_, _, typeArr)) ainit' ado' =
  case typeArr of
    TypeArray a ->
      case ainit' of
        Left _ -> return . Left $ forDef'' axs' ainit' ado' a
        Right (_, _, acc) ->
          Right <$> forDef'' axs' ainit' ado' a acc
    x -> strError $ "Expected any kind of list, but got: " ++ repr x
forDef' _ _ _ = strError "Couldn't infer type of array. Introduce a variable to change it."

forDef'' :: Exp' -> Exp' -> Exp' -> DataType -> DataType -> Alex RefCodeDt
forDef'' axs ainit ado a acc = do
  (refArr, codeArr, _) <- checkType (TypeArray a) axs
  (refInit, codeInit, _) <- checkType acc ainit
  (refAdo, codeAdo, _) <- checkType (TypeFun [acc, a, acc]) ado
  refAcc <- getRef <&> RefVar
  refIt <- getRef <&> RefVar
  refMaxIt <- getRef <&> RefVar
  bucle <- getRef
  endFor <- getRef
  boolExp <- getRef <&> RefVar
  return
    ( refAcc,
      codeArr ++ codeInit ++ codeAdo
        ++ TacCopy refAcc refInit :
      TacCopy refIt (RefConstInt 1) :
      TacOp refMaxIt (RefInf refArr (RefConstInt 0)) OpMult (RefConstInt $ sizeof a) :
      TacOp refMaxIt refMaxIt OpSum (RefConstInt 1) :
      TacLabel bucle :
      TacOp boolExp refIt OpLt refMaxIt :
      TacIfExp boolExp endFor :
      TacPushParam (RefInf refArr refIt) :
      TacPushParam refAcc :
      TacCall refAdo :
      TacCopy refAcc RefSP :
      TacOp refIt refIt OpSum (RefConstInt (sizeof a)) :
      TacGoto bucle :
      [TacLabel endFor],
      acc
    )

maybeHead :: [a] -> Maybe a
maybeHead (x : _) = Just x
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
  ref <- getRef <&> RefVar
  let refSup = RefInf ref
  return
    ( ref,
      concatMap snd' xs'
        ++ TacCopy (refSup $ RefConstInt 0) (RefConstInt (length xs)) :
      zipWith
        (TacCopy . refSup . RefConstInt)
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
               TacCall $ RefVar nameCond,
               TacIfExp RefSP out,
               TacPushParam refAcc,
               TacCall $ RefVar nameFuncAcc,
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
               TacCall $ RefVar nameFuncAcc,
               TacCopy refAcc RefSP,
               TacPushParam refAcc,
               TacCall $ RefVar nameCond,
               TacIfExp RefSP bucle
             ],
        dtype
      )

getStringOfRef :: Ref -> Alex String
getStringOfRef ref = case ref of
  RefVar n -> return n
  RefFunc n -> return n
  _ -> strError "Expected: Function, Got: Constant"

getArrayElemDef :: Exp -> Exp -> Exp
getArrayElemDef pos array = do
  posExp <- pos >>= checkType TypeInt
  arrExp <- array
  case arrExp of
    Right (_, _, TypeArray dt) ->
      Right <$> getArrayElemDef' posExp arrExp dt
    Right (_, _, dt) -> strError $ "Expected array, but got: " ++ repr dt
    Left _ -> return . Left $ getArrayElemDef' posExp arrExp

getArrayElemDef' :: RefCodeDt -> Exp' -> DataType -> Alex RefCodeDt
getArrayElemDef' (refPos, codePos, _) arrExp dt = do
  (refArr, codeArr, _) <- checkType (TypeArray dt) arrExp
  pos <- getRef <&> RefVar
  return
    ( RefInf refArr pos,
      codePos ++ codeArr
        ++ TacOp pos refPos OpMult (RefConstInt $ sizeof dt) :
      [TacOp pos pos OpSum (RefConstInt 1)],
      dt
    )

hasDefinition :: Name -> [Map.Map String DataDef] -> Alex DataDef
hasDefinition name maps
  | null xs = strError $ "Name " ++ repr name ++ " is not defined. This shoudln't be possible."
  | otherwise = return . (Map.! name) $ head xs
  where
    xs = dropWhile (not . Map.member name) maps

isEmpty :: (Show k, Show v) => Map.Map k v -> Alex ()
isEmpty m
  | Map.null m = return ()
  | otherwise = strError $ "Case of doesn't have all the definitions: " ++ show m

defCaseOf :: Exp -> [DataType -> CaseAcc -> Alex CaseAcc] -> DataType -> Alex RefCodeDt
defCaseOf e c d = do
  e' <- e
  defCaseOf' e' c d

defCaseOf' :: Exp' -> [DataType -> CaseAcc -> Alex CaseAcc] -> DataType -> Alex RefCodeDt
defCaseOf' (Right (refCase, codeCase, TypeDef name)) cases expected = do
  s <- alexGetUserState
  DataDef _ multDefs <- hasDefinition name (s ^. definitions)
  let f (MultDef n xs) i = (n, (xs, i))
  let defs = Map.fromList $ zipWith f multDefs [0 ..]
  (shouldBeEmpty, _, res) <- foldl (>>=) (return (defs, Set.empty, Map.empty)) $ map ($ expected) cases
  isEmpty shouldBeEmpty
  refRes <- getRef <&> RefVar
  let codeCases = foldr (\(_, c) acc -> c ++ acc) [] $ Map.elems res :: Code
  let xs = map (\(x, (ref, _)) -> (x, ref)) $ Map.toAscList res
  defType <- getRef <&> RefVar
  endCase <- getRef
  whichCode' <-
    traverse
      ( \(i, callFunc) ->
          getRef <&> RefVar >>= \s' -> getRef >>= \label ->
            return ( [ TacLabel label
                     , TacPushParam refCase
                     , TacCall (RefVar callFunc)
                     , TacGoto endCase
                     ]
                   , [ TacOp s' defType OpNeq (RefConstInt i),
                       TacIfExp s' label
                     ]
                   )
      )
      xs
  let whichCode = concatMap snd whichCode' :: Code
  let callWhichCode = concatMap fst whichCode' :: Code
  refTemp <- getRef
  return
    ( refRes,
      codeCase ++ TacGoto refTemp : codeCases ++
      [ TacLabel refTemp
      , TacCopy defType (RefInf refCase $ RefConstInt 0)
      ] ++ whichCode
    ++ callWhichCode ++
      [ TacLabel endCase
      , TacCopy refRes RefSP
      ]
    , expected
    )
defCaseOf' (Right (_, _, dt)) _ _ = strError $ "Expected data definition for a case of, but got " ++ repr dt
defCaseOf' _ _ _ = strError "Couldn't infer type of case. Add a local variable"

defCase :: Name -> Exp' -> DataType -> CaseAcc -> Alex CaseAcc
defCase name fun dt (defs, alreadyDef, res)
  | Set.member name alreadyDef = strError $ "Name " ++ name ++ " is already catched in case of"
  | not (Map.member name defs) = strError $ "Name " ++ name ++ " is not a valid definition"
  | otherwise = do
    let (def, i) = defs Map.! name
    let defs' = Map.delete name defs
    let alreadyDef' = Set.insert name alreadyDef
    let dt' = TypeFun $ init def ++ [dt]
    (ref, code, _) <- checkType dt' fun
    refLabel <- getRef
    otherRef <- getRef <&> RefVar
    let nums = init . scanl (+) 1 . map sizeof $ init def
    let res' =
          Map.insert
            i
            ( refLabel,
              code ++ TacFuncLabel refLabel :
              TacGetParam otherRef 0 :
              reverse
                ( map
                    (TacPushParam . RefInf otherRef . RefConstInt)
                    nums
                )
                ++ [TacCall ref, TacReturn RefSP]
            )
            res
    return (defs', alreadyDef', res')
