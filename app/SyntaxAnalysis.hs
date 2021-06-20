-- | Performs type analysis on the syntax tree.
module SyntaxAnalysis where

import AlexUserState
import Control.Monad (zipWithM)
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lexer
import ParserData

type Exp' = Alex (Ref, [ThreeAddressCode])

type Exp = DataType -> Exp'

-- * Utility Functions

guardedExp :: DataType -> Exp' -> Exp
guardedExp d m d'
  | d /= d' = strError $ "Guarded\nExpected " ++ repr d ++ " but found " ++ repr d'
  | otherwise = m

happyError :: Show s => s -> Alex a
happyError = strError . show

strError :: String -> Alex a
strError tok = do
  x <- getLineAndColumn
  alexError $ "Happy error on line and column " ++ show x ++ ": " ++ tok

-- | Wraps function definition
wrapFunction :: [Name] -> Name -> Name -> [ThreeAddressCode] -> Ref -> (Ref, [ThreeAddressCode])
wrapFunction params funcName later code ref =
  ( RefFunc funcName,
    TacGoto later :
    TacFuncLabel funcName :
    zipWith TacGetParam (map RefVar params) [1 ..]
      ++ code
      ++ [ TacReturn ref,
           TacLabel later
         ]
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
removeContext = alexGetUserState >>= removeContext'

removeContext' :: AlexUserState -> Alex AlexUserState
removeContext' a@(AlexUserState vs ds _)
  | length vs == 1 = strError "Invalid State: Removed values context when there was only one left."
  | length ds == 1 = strError "Invalid State: Removed definitions context when there was only one left."
  | otherwise = return . over values tail $ over definitions tail a

-- * Parser.y functions

getNameParam :: Name -> DataType -> Exp'
getNameParam name dt = do
  s <- alexGetUserState
  let vdicc = s ^. values
  if any (Map.member name) vdicc
    then getNameParam' name dt
    else strError $ "Name " ++ name ++ " is not defined. Expected type: " ++ repr dt

getNameParam' ::
  -- | Name of the function
  Name ->
  Exp
getNameParam' name dt = do
  s <- alexGetUserState
  let vdicc = s ^. values
  let val = (Map.! name) . head $ dropWhile (not . Map.member name) vdicc
  guardedExp val (return (RefVar name, [])) dt

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
  dType <- dtype'
  s <- alexGetUserState
  alexSetUserState $
    over
      values
      -- TODO canviar el valor generat.
      (\(x : xs) -> Map.insert name dType x : xs)
      s
  return def

getExp :: DataType -> Exp -> Op -> Exp -> DataType -> Exp'
getExp dt e op e' dt'
  | dt /= dt' = strError $ "Needed " ++ repr dt ++ " but found " ++ repr dt'
  | otherwise = do
    ref <- getRef <&> RefVar
    (r, code) <- e dt
    (r', code') <- e' dt
    return (ref, code ++ code' ++ [TacOp ref r op r'])

getExp' :: DataType -> DataType -> Exp -> Op -> Exp -> DataType -> Exp'
getExp' dtRes dtDefs e op e' dt'
  | dtRes /= dt' = strError $ "Needed " ++ repr dtRes ++ " but found " ++ repr dt'
  | otherwise = do
    ref <- getRef <&> RefVar
    (r, code) <- e dtDefs
    (r', code') <- e' dtDefs
    return (ref, code ++ code' ++ [TacOp ref r op r'])


getUnaryExp :: DataType -> UnaryOp -> Exp -> DataType -> Exp'
getUnaryExp dt op e' dt'
  | dt /= dt' = strError $ "Needed " ++ repr dt ++ " but found " ++ repr dt'
  | otherwise = do
    ref <- getRef <&> RefVar
    (r', code') <- e' dt
    return (ref, code' ++ [TacUnary ref op r'])

applyFunc :: Name -> [DataType -> Alex (Ref, [ThreeAddressCode])] -> DataType -> Alex (Ref, [ThreeAddressCode])
applyFunc name params dtype = do
  s <- alexGetUserState
  let vdicc = s ^. values
  if any (Map.member name) vdicc
    then do
      let val = (Map.! name) . head $ dropWhile (not . Map.member name) vdicc
      case val of
        TypeFun xs ->
          if length xs < length params
            then
              strError $
                "Applied more arguments than needed:"
                  ++ "\n\t\tApplied: "
                  ++ show (length params)
                  ++ "\n\t\t    Got: "
                  ++ show (length xs)
            else do
              appliedParams <- zipWithM ($) params xs
              if length xs - 1 == length params
                then do
                  r <- getRef <&> RefVar
                  return
                    ( r,
                      reverse
                        ( TacCopy r RefSP :
                          TacCall name :
                          map (TacPushParam . fst) appliedParams
                            ++ concatMap snd appliedParams
                        )
                    )
                else do
                  funcName <- getRef
                  later <- getRef
                  -- Idea:
                  --   goto later; Func funcName [Def ...] label later;
                  return
                    ( RefFunc funcName,
                      TacGoto later :
                      TacFuncLabel funcName :
                      concatMap snd appliedParams
                        ++ map (TacPushParam . fst) appliedParams
                        ++ [ TacCall name,
                             TacReturn RefSP,
                             TacLabel later
                           ]
                    )
        x ->
          if null params -- Synonim
            then
              if x == dtype
                then return (RefVar name, [])
                else
                  strError $
                    "ApplyFunc: Expected value and returned differ: "
                      ++ "\n\t\tExpected: "
                      ++ show dtype
                      ++ "\n\t\tReturned: "
                      ++ show x
            else
              strError
                ( "Applied Function to value: "
                    ++ "\n\t\t               Value: "
                    ++ show x
                    ++ "\n\t\tNumber of parameters: "
                    ++ show (length params)
                )
    else strError $ "Apply Function name " ++ name ++ " is not defined"

-- | Define Func only for Outer Assigns
defineFunc ::
  -- |  Name of the function
  Name ->
  -- |  Types of fun
  [Alex DataType] ->
  -- |  Def
  (DataType -> Alex (Ref, [ThreeAddressCode])) ->
  Alex [ThreeAddressCode]
defineFunc name [x'] _ = do
  x <- x'
  strError $ "Can not have global constants, please declare using unit: " ++ name ++ ":: () -> " ++ repr x ++ " = definition"
defineFunc name dto f = do
  x' <- sequenceA dto
  let x'' = TypeFun $ reverse x'
  s <- alexGetUserState
  alexSetUserState $ over values (\(y : ys) -> Map.insert name x'' y : ys) s
  (ref, code) <- f x''
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
  (DataType -> Alex (Ref, [ThreeAddressCode])) ->
  Alex [ThreeAddressCode]
defineFunc' name [x] f = do
  x' <- x
  s <- alexGetUserState
  alexSetUserState $ over values (\(y : ys) -> Map.insert name x' y : ys) s
  (ref, code) <- f x'
  return $ code ++ [TacCopy (RefVar name) ref]
defineFunc' a b c = defineFunc a b c

functionDef :: [Name] -> Alex [ThreeAddressCode] -> (DataType -> Alex (Ref, [ThreeAddressCode])) -> DataType -> Alex (Ref, [ThreeAddressCode])
functionDef names mcode def (TypeFun xs)
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
      (ref, code') <- def (if length xs' == 1 then head xs' else TypeFun xs')
      _ <- removeContext
      return $ wrapFunction names funcName later (code ++ code') ref
functionDef [] mcode def x =
  do
    _ <- newContext
    funcName <- getRef
    later <- getRef
    code <- mcode
    (ref, code') <- def x
    _ <- removeContext
    return $ wrapFunction [] funcName later (code ++ code') ref
functionDef _ _ _ _ = strError "Invalid definition of a function."

-- while :: (acc -> Bool) -> (acc -> acc) -> acc -> acc
-- if Bool a a

type Statements = Alex [ThreeAddressCode]

defineConditional :: Exp -> Statements -> Exp -> Statements -> Exp -> Exp
defineConditional boolExp stsThen expThen stsElse expElse dtype =
  do
    (refBool, boolCode) <- boolExp TypeBool
    _ <- newContext
    codeThen <- stsThen
    (refThen, codeThen') <- expThen dtype
    _ <- removeContext
    _ <- newContext
    codeElse <- stsElse
    (refElse, codeElse') <- expElse dtype
    _ <- removeContext
    labelElse <- getRef
    res <- getRef <&> RefVar
    labelFinal <- getRef
    return
      ( res,
        boolCode
          ++ TacIfExp refBool labelElse :
        codeThen ++ codeThen' ++
        TacCopy res refThen :
        TacGoto labelFinal :
        TacLabel labelElse :
        codeElse ++ codeElse'
          ++ [ TacCopy res refElse,
               TacLabel labelFinal
             ]
      )
