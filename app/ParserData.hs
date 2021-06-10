-- | Module where it contains the Parser data
-- This module shouldn't have anything to execute, only define the data.
{-# LANGUAGE TemplateHaskell #-}
module ParserData where

import qualified Data.Map.Strict               as Map
import           Lens.Micro
import           Lens.Micro.TH
import           Data.Bits

type Val = Either Double Int

type Name = String

-- | Type
data DataType =
  TypeBool
  | TypeInt
  | TypeReal
  | TypeDef Name
  | TypeFun [DataType]
  deriving (Show, Eq, Ord, Read)

data MultDef = MultDef { multName :: Name
                       -- | It's either named or unnamed.
                       , parameters :: [DataType]
                       }
      deriving (Show, Eq, Ord, Read)

-- | Data constructor. First parameter
data DataDef = DataDef Name [MultDef] -- ^ Sum types
  deriving (Show, Eq, Ord, Read)

data Exp = TNone
  -- | Assign Expressions
  | DataStatement DataDef
  | DefFunc Name Exp Exp
  -- | Math expressions
  | TSum Exp Exp
  | TMinus Exp Exp
  | TMult Exp Exp
  | TDiv Exp Exp
  | TDivInt Exp Exp
  | TLeftShift Exp Exp
  | TRightShift Exp Exp
  | TVal Int
  | TRealVal Double
  | TBrack Exp
  | TRealAssign Char Exp
  | TIntAssign Char Exp
  | TRealGet Char
  | TIntGet Char
  | TMod Exp Exp
  | TNegate Exp
  | TPositive Exp
  | TCompAUn Exp
  | TAnd Exp Exp
  | TOr Exp Exp
  | TXor Exp Exp 
  | TIntToReal Exp
  | TRealToInt Exp
  deriving (Show, Read, Eq, Ord)

{-
{-- | Evaluates GADT mantaining the state
-- This functions lets avaluate an Alex (Exp a) to an
-- Alex a using GADTs powerful type system.
--
-- # Examples of usage
-- >>> runAlex "" $ eval . return $ TVal 3
--
-- >>> runAlex "" $ const alexGetUserState =<< (eval $ return $ TIntAssign 'a' $ TSum (TVal 3) (TVal 4))
-}
eval :: Exp -> Alex Val
eval outer =
  case outer of
    TSum ea eb -> do
      a <- eval ea
      b <- eval eb
      liftOperator (+) (+) a b
    TMinus ea eb -> do
      a <- eval ea
      b <- eval eb
      liftOperator (-) (-) a b
    TMult ea eb -> do
      a <- eval ea
      b <- eval eb
      liftOperator (*) (*) a b
    TDiv ea eb -> do
      a <- eval ea
      b <- eval eb
      ifBothLeft (/) a b
    TVal a -> return $ Right a
    TRealVal a -> return $ Left a
    TBrack a -> eval  a
    TRealAssign c expDouble -> do
      s <- alexGetUserState
      b' <- eval expDouble
      case b' of
        Left b -> do
                  alexSetUserState $ over reals (Map.insert c b) s
                  return $ Left b
        Right _ -> do
                   (line, column) <- getLineAndColumn
                   alexError $ "Trying to assign integer at double variable at " ++ show line ++ ':': show column
    TIntAssign c expInt -> do
      s <- alexGetUserState
      b' <- eval expInt
      case b' of
        Right b -> do
                  alexSetUserState $ over integers (Map.insert c b) s
                  return $ Right b
        Left _ -> do
                   (line, column) <- getLineAndColumn
                   alexError $ "Trying to assign double at integer variable at " ++ show line ++ ':': show column
    TRealGet c -> do
      s <- alexGetUserState
      if c `Map.member` (s ^. reals)
        then return . Left $ (s ^. reals) Map.! c
        else do
          (line, column) <- getLineAndColumn
          alexError $ "Character "
                      ++ show c
                      ++ " is not a part of the real definitions  now: "
                      ++ show (s ^. reals)
                      ++ ", at" ++ show line ++ ':': show column
    TIntGet c -> do
      s <- alexGetUserState
      if c `Map.member` (s ^. integers)
        then return . Right $ (s ^. integers) Map.! c
        else do
          (line, column) <- getLineAndColumn
          alexError $ "Character "
                      ++ show c
                      ++ " is not a part of the integers definitions now: "
                      ++ show (s ^. integers)
                      ++ ", at" ++ show line ++ ':': show column
    TMod exp exp' -> do
      a <- eval exp
      b <- eval exp'
      ifBothRights mod a b
    TNegate exp -> do
      a <- eval exp
      return $ fmap negate a
    TPositive exp -> eval exp
    TRightShift exp exp' -> do
      a <- eval exp
      b <- eval exp'
      ifBothRights  shiftR a b
    TLeftShift exp exp' -> do
      a <- eval exp
      b <- eval exp'
      ifBothRights shiftL a b
    TCompAUn exp -> do
      a <- eval exp
      ifIsRight complement a
    TAnd exp exp' -> do
      a <- eval exp
      b <- eval exp'
      ifBothRights (.&.) a b
    TOr exp exp' -> do
      a <- eval exp
      b <- eval exp'
      ifBothRights (.|.) a b
    TXor exp exp' -> do
      a <- eval exp
      b <- eval exp'
      ifBothRights xor a b
    TIntToReal exp -> do
      a <- eval exp
      intToreal a
    TRealToInt exp -> do
      a <- eval exp
      realToInt a
    TDivInt exp exp' -> do
      a <- eval exp
      b <- eval exp'
      ifBothRights div a b 

realToInt :: Val -> Alex Val
realToInt (Left a) = return . Right $ floor a  
realToInt _ = do
  (line, column) <- getLineAndColumn
  alexError
    $ "Cannot apply cast from int to int, the expression is not a real "
    ++ show line
    ++ ':'
    :  show column

intToreal :: Val -> Alex Val
intToreal (Right a) = return . Left $ fromIntegral a
intToreal _ = do
  (line, column) <- getLineAndColumn
  alexError
    $ "Cannot apply cast from real to real, the expression is not an int "
    ++ show line
    ++ ':'
    :  show column

-- | Lifts two operators at a value level
-- ===Exemple
-- >>> runAlex "" $ liftOperator (+) (+) (Left 3) (Left 3)
-- Right (Left 6)
liftOperator
  :: (Int -> Int -> Int)
  -> (Double -> Double -> Double)
  -> Val
  -> Val
  -> Alex Val
liftOperator _ f (Left  x) (Left  y) = return . Left $ f x y
liftOperator f _ (Right x) (Right y) = return . Right $ f x y
liftOperator _ _ _         _         = do
  (line, column) <- getLineAndColumn
  alexError
    $ "Cannot apply operator between doubles and integers. Please cast to either one of them! At "
    ++ show line
    ++ ':'
    :  show column

-- | Lifts one operator if both are integers
-- ===Exemple
-- >>> runAlex "" $ ifBothRights (+) (Right 3) (Right 3)
-- Right (Right 6)
ifBothRights :: (Int -> Int -> Int) -> Val -> Val -> Alex Val
ifBothRights f (Right x) (Right y) = return . Right $ f x y
ifBothRights _ _         _         = do
  (line, column) <- getLineAndColumn
  alexError
    $ "Cannot apply operator between doubles and integers. Please cast to either one of them! At "
    ++ show line
    ++ ':'
    :  show column

-- | Lifts one operator if the argument is an integer
-- ===Exemple
-- >>> runAlex "" $ ifBothRights (+) (Right 3)
-- Right (Right 3)
ifIsRight :: (Int -> Int) -> Val -> Alex Val
ifIsRight f (Right x) = return . Right $ f x
ifIsRight _ _ = do
  (line, column) <- getLineAndColumn
  alexError $ "Cannot apply operator to any other type that isn't Int "
            ++ show line ++ ':': show column

-- | Lifts one operator if the argument is a Double
-- ===Exemple
-- >>> runAlex "" $ ifBothRights (+) (Right 3.5)
-- Right (Right 3.5)
ifIsLeft :: (Double -> Double) -> Val -> Alex Val
ifIsLeft f (Left x) = return . Left $ f x
ifIsLeft _ _ = do
  (line, column) <- getLineAndColumn
  alexError $ "Cannot apply operator to any other type that isn't Double"
            ++ show line ++ ':': show column

-- | Lifts one operator if both are doubles
-- ===Exemple
-- >>> runAlex "" $ ifBothLeft (+) (Right 3) (Right 3)
-- Right (Left 6)
ifBothLeft :: (Double -> Double -> Double) -> Val -> Val -> Alex Val
ifBothLeft f (Left x) (Left y) = return . Left $ f x y
ifBothLeft _ _        _        = do
  (line, column) <- getLineAndColumn
  alexError
    $ "Cannot apply operator between doubles and integers. Please cast to either one of them! At "
    ++ show line
    ++ ':'
    :  show column
-}
