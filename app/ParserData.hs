{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Module where it contains the Parser data
-- This module shouldn't have anything to execute, only define the data.
module ParserData where

import Data.List (intercalate)

class Repr a where
  repr :: a -> String

instance Repr Int where
  repr = show



type Name = String

-- | Type
data DataType
  = TypeBool
  | TypeInt
  | TypeReal
  | TypeChar
  | TypeUnit
  | TypeDef Name
  | TypeArray DataType
  | TypeFun [DataType]
  deriving (Show, Eq, Ord, Read)

instance Repr DataType where
  repr dt = case dt of
    TypeBool -> "Bool"
    TypeInt -> "Int"
    TypeChar -> "Char"
    TypeReal -> "Real"
    TypeDef n -> n
    TypeUnit -> "()"
    TypeFun xs -> intercalate " -> " $ map repr xs
    TypeArray x -> '[' : repr x ++ "]"

-- TODO posar el nombre de bytes correctes.
sizeof :: DataType -> Int
sizeof x = case x of
  TypeBool -> 1
  TypeInt -> 2
  TypeReal -> 3
  TypeUnit -> 1
  TypeChar -> 1
  TypeDef _ -> 1
  TypeFun _ -> 1
  TypeArray _ -> 1

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

data Op
  = OpSum
  | OpMult
  | OpMinus
  | OpDiv
  | OpLeftShift
  | OpRightShift
  | OpMod
  | OpEq
  | OpNeq
  | OpLt
  | OpLEq
  | OpGt
  | OpGEq
  | OpBitAnd
  | OpBitOr
  | OpBitXOR
  | OpOr
  | OpAnd
  | OpXOR
  deriving (Show, Read, Eq, Ord)

instance Repr Op where
  repr op = case op of
    OpSum -> "+"
    OpMult -> "*"
    OpMinus -> "-"
    OpDiv -> "//"
    OpLeftShift -> "<<"
    OpRightShift -> ">>"
    OpMod -> "%"
    OpEq -> "=="
    OpNeq -> "!="
    OpLt -> "<"
    OpLEq -> "<="
    OpGt -> ">"
    OpGEq -> ">="
    OpBitAnd -> "&"
    OpBitOr -> "|"
    OpBitXOR -> "^"
    OpOr -> "||"
    OpXOR -> "XOR"
    OpAnd -> "&&"

data UnaryOp = UnaryMinus | UnaryComplement | UnaryNot
  deriving (Show, Eq, Read, Ord)

instance Repr UnaryOp where
  repr op = case op of
    UnaryMinus -> "-"
    UnaryComplement -> "~"
    UnaryNot -> "¬"

data Ref
  = RefVar String
  | RefInf Ref Ref -- a @ 0
  | RefConstInt Int
  | RefConstReal Double
  | RefConstBool Bool
  | RefSP
  | RefFunc Name
  deriving (Show, Eq, Read, Ord)

instance Repr Ref where
  repr a = case a of
    RefVar ref -> ref
    RefInf ref i -> repr ref ++ " @ " ++ repr i
    RefConstInt r -> show r
    RefConstReal r -> show r
    RefConstBool r -> show r
    RefSP -> "$SP"
    RefFunc n -> n

instance Repr String where
  repr = id

type Label = String

unit :: Ref
unit = RefConstInt 0

data ThreeAddressCode
  = TacHalt
  | -- | Math expressions
    TacOp Ref Ref Op Ref
  | TacCopy Ref Ref
  | TacUnary Ref UnaryOp Ref
  | TacLabel Label
  | TacFuncLabel Label
  | TacGetParam Ref Int -- a := param 1
  | TacPushParam Ref
  | TacIfExp Ref Label -- if false ref goto label
  | TacReturn Ref
  | TacCall Ref
  | TacDefCode Ref ThreeAddressCode
  | TacGoto Label
  deriving (Show, Eq, Ord, Read)

prefix :: String
prefix = replicate 4 ' '

instance Repr ThreeAddressCode where
  repr v = case v of
    TacHalt -> "halt"
    TacOp r a op b -> prefix ++ repr r ++ " := " ++ repr a ++ " " ++ repr op ++ " " ++ repr b
    TacCopy r a -> prefix ++ repr r ++ " := " ++ repr a
    TacUnary r op a -> prefix ++ repr r ++ " := " ++ repr op ++ " " ++ repr a
    TacLabel l -> "Label " ++ l
    TacFuncLabel l -> "Func " ++ l
    TacGetParam r i -> prefix ++ repr r ++ " := param " ++ show i
    TacPushParam r -> prefix ++ "param " ++ repr r
    TacIfExp r l -> prefix ++ "if false " ++ repr r ++ " goto " ++ l
    TacReturn r -> prefix ++ "return " ++ repr r
    TacCall s -> prefix ++ "call " ++ repr s
    TacDefCode r c -> prefix ++ repr r ++ " = (" ++ repr c ++ ")"
    TacGoto l -> prefix ++ "goto " ++ l
