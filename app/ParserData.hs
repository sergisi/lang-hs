{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Module where it contains the Parser data
-- This module shouldn't have anything to execute, only define the data.
module ParserData where

import Data.Bits
import qualified Data.Map.Strict as Map
import Lens.Micro
import Lens.Micro.TH

class Repr a where
  repr :: a -> String

instance Repr Int where
  repr = show

type Val = Either Double Int

type Name = String

-- | Type
data DataType
  = TypeBool
  | TypeInt
  | TypeReal
  | TypeChar
  | TypeUnit
  | TypeDef Name
  | TypeFun [DataType]
  deriving (Show, Eq, Ord, Read)

instance Repr DataType where
  repr dt = case dt of
    TypeBool -> "Bool"
    TypeInt -> "Int"
    TypeChar -> "Char"
    TypeDef n -> n
    TypeFun xs -> foldl (\acc x -> acc ++ " -> " ++ repr x) "" $ map repr xs

-- TODO posar el nombre de bytes correctes.
sizeof :: DataType -> Int
sizeof x = case x of
  TypeBool -> 1
  TypeInt -> 2
  TypeReal -> 3
  TypeChar -> 1
  TypeDef _ -> 1
  TypeFun _ -> 1

data MultDef = MultDef
  { multName :: Name,
    -- | It's either named or unnamed.
    parameters :: [DataType]
  }
  deriving (Show, Eq, Ord, Read)

-- | Data constructor. First parameter
data DataDef
  = -- | Sum types
    DataDef Name [MultDef]
  deriving (Show, Eq, Ord, Read)

-- TODO Delete
data Exp
  = TNone
  | -- | Assign Expressions
    DataStatement DataDef
  | DefFunc Name Exp Exp
  deriving (Show, Read, Eq, Ord)

data IntOp
  = IntOpSum
  | IntOpMult
  | IntOpMinus
  | IntOpDiv
  | IntOpLeftShift
  | IntOpRightShift
  | IntOpMod
  | IntOpEq
  | IntOpNeq
  | IntOpLt
  | IntOpLEq
  | IntOpGt
  | IntOpGEq
  | IntOpBitAnd
  | IntOpBitOr
  | IntOpBitXOR
  deriving (Show, Read, Eq, Ord)

data RealOp
  = RealOpSum
  | RealOpMult
  | RealOpMinus
  | RealOpDiv
  | RealOpEq
  | RealOpNeq
  | RealOpLt
  | RealOpLEq
  | RealOpGt
  | RealOpGEq
  deriving
    ( Show,
      Read,
      Eq,
      Ord
    )

data BoolOp = BoolOpOr | BoolOpAnd | BoolOpXOR | BoolOpEq | BoolOpNeq
  deriving (Show, Read, Eq, Ord)

instance Repr IntOp where
  repr op = case op of
    IntOpSum -> "+"
    IntOpMult -> "*"
    IntOpMinus -> "-"
    IntOpDiv -> "//"
    IntOpLeftShift -> "<<"
    IntOpRightShift -> ">>"
    IntOpMod -> "%"
    IntOpEq -> "=="
    IntOpNeq -> "!="
    IntOpLt -> "<"
    IntOpLEq -> "<="
    IntOpGt -> ">"
    IntOpGEq -> ">="
    IntOpBitAnd -> "&"
    IntOpBitOr -> "|"
    IntOpBitXOR -> "^"

instance Repr RealOp where
  repr op = case op of
    RealOpSum -> "+"
    RealOpMult -> "*"
    RealOpMinus -> "-"
    RealOpDiv -> "/"
    RealOpEq -> "=="
    RealOpNeq -> "!="
    RealOpLt -> "<"
    RealOpLEq -> "<="
    RealOpGt -> ">"
    RealOpGEq -> ">="

instance Repr BoolOp where
  repr op = case op of
    BoolOpOr -> "||"
    BoolOpXOR -> "XOR"
    BoolOpAnd -> "&&"
    BoolOpEq -> "=="
    BoolOpNeq -> "!="

data UnaryIntOp = UnaryIntMinus | UnaryIntComplement
  deriving (Show, Eq, Read, Ord)

data UnaryRealOp = UnaryRealMinus
  deriving (Show, Eq, Read, Ord)

data UnaryBoolOp = UnaryBoolNot
  deriving (Show, Eq, Read, Ord)

instance Repr UnaryIntOp where
  repr op = case op of
    UnaryIntMinus -> "-"
    UnaryIntComplement -> "~"

instance Repr UnaryRealOp where
  repr _ = "-"

instance Repr UnaryBoolOp where
  repr _ = "¬"

data Ref
  = RefVar String
  | RefInf String Int -- a @ 0
  | RefInt TacInt
  | RefReal TacReal
  | RefBool TacBool
  | RefSP
  | RefFunc Name
  deriving (Show, Eq, Read, Ord)

instance Repr Ref where
  repr a = case a of
    RefVar ref -> ref
    RefInf ref i -> ref ++ " @ " ++ show i
    RefInt r -> repr r
    RefReal r -> repr r
    RefBool r -> repr r
    RefSP -> "$SP"
    RefFunc n -> n

data TacReal = ConstantReal Double | RealRef Ref
  deriving (Show, Eq, Read, Ord)

data TacInt = ConstantInt Int | IntRef Ref
  deriving (Show, Eq, Read, Ord)

data TacBool = ConstantBool Bool | BoolRef Ref
  deriving (Show, Eq, Read, Ord)

instance Repr TacBool where
  repr v = case v of
    ConstantBool b -> show b
    BoolRef ref -> repr ref

instance Repr TacInt where
  repr v = case v of
    ConstantInt i -> show i
    IntRef ref -> repr ref

instance Repr TacReal where
  repr v = case v of
    ConstantReal d -> show d
    RealRef ref -> repr ref

instance Repr String where
  repr = id

type Label = String

unit :: Ref
unit = RefInt (ConstantInt 0)

data ThreeAddressCode
  = TacHalt
  | -- | Math expressions
    TacIntOp Ref TacInt IntOp TacInt
  | TacRealOp Ref TacReal RealOp TacReal
  | TacBoolOp Ref TacBool BoolOp TacBool
  | TacBoolCopy Ref TacBool
  | TacIntCopy Ref TacInt
  | TacRealCopy Ref TacReal
  | TacIntUnary Ref UnaryIntOp TacInt
  | TacBoolUnary Ref UnaryBoolOp TacBool
  | TacRealUnary Ref UnaryRealOp TacReal
  | TacCopy Ref Ref
  | TacLabel Label
  | TacFuncLabel Label
  | TacGetParam Ref Int -- a := param 1
  | TacPushParam Ref
  | TacIfExp Ref Label -- if false ref goto label
  | TacReturn Ref
  | TacCall Name
  | TacDefCode Ref ThreeAddressCode
  | TacGoto Label
  deriving (Show, Eq, Ord, Read)

prefix :: String
prefix = replicate 4 ' '

instance Repr ThreeAddressCode where
  repr v = case v of
    TacHalt -> "halt"
    TacIntOp r a op b -> prefix ++ repr r ++ " := " ++ repr a ++ " " ++ repr op ++ " " ++ repr b
    TacRealOp r a op b -> prefix ++ repr r ++ " := " ++ repr a ++ " " ++ repr op ++ " " ++ repr b
    TacBoolOp r a op b -> prefix ++ repr r ++ " := " ++ repr a ++ " " ++ repr op ++ " " ++ repr b
    TacIntCopy r a -> prefix ++ repr r ++ " := " ++ repr a
    TacBoolCopy r a -> prefix ++ repr r ++ " := " ++ repr a
    TacCopy r a -> prefix ++ repr r ++ " := " ++ repr a
    TacRealCopy r a -> prefix ++ repr r ++ " := " ++ repr a
    TacIntUnary r op a -> prefix ++ repr r ++ " := " ++ repr op ++ " " ++ repr a
    TacBoolUnary r op a -> prefix ++ repr r ++ " := " ++ repr op ++ " " ++ repr a
    TacRealUnary r op a -> prefix ++ repr r ++ " := " ++ repr op ++ " " ++ repr a
    TacLabel l -> "Label " ++ l
    TacFuncLabel l -> "Func " ++ l
    TacGetParam r i -> prefix ++ repr r ++ " := param " ++ show i
    TacPushParam r -> prefix ++ "param " ++ repr r
    TacIfExp r l -> prefix ++ "if false " ++ repr r ++ " goto " ++ l
    TacReturn r -> prefix ++ "return " ++ repr r
    TacCall s -> prefix ++ "call " ++ s
    TacDefCode r c -> prefix ++ repr r ++ " = (" ++ repr c ++ ")"
    TacGoto l -> prefix ++ "goto " ++ l
