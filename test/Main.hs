-- |
module Main where

import AlexUserState
import Control.Monad
  ( liftM,
    liftM2,
    when,
  )
import Data.Map.Strict as Map
import qualified Data.ByteString.Lazy as LBS
import Lexer
import Parser
import ParserData
import SyntaxAnalysis
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden (goldenVsString, findByExtension)

funcTests =
  testGroup
    "Creates functions accordingly"
    [ testCase "Constructor with 3 parameters" $
        expected @=? constructor
    ]
  where
    constructor = runAlex "" $ generateMultDef (MultDef "name" [TypeInt, TypeChar, TypeReal, TypeFun [TypeInt, TypeBool], TypeBool]) 0
    expected = Right [TacFuncLabel "name", TacIntCopy (RefInf "temp0" 0) (ConstantInt 0), TacGetParam (RefInf "temp0" 1) 0, TacGetParam (RefInf "temp0" 3) 1, TacGetParam (RefInf "temp0" 4) 2, TacGetParam (RefInf "temp0" 7) 3, TacReturn (RefVar "temp0")]

dataTests =
  testGroup
    "Data tests"
    [ testCase "Normal test" $ expected @=? runExpression' "data Ha = Ha Int (Real -> Bool) | Haha;",
      testCase "List test" $ expected' @=? runExpression' "data List = List Int List | Empty;",
      testCase "Name not defined" $ expected'' @=? runExpression' "data A = A B",
      testCase "Name already defined" $ alreadyDefined @=? runExpression' "data Ha = Ha;data Ha = Ha;"
    ]
  where
    expected = Right AlexUserState {_values = [fromList [("Ha", Value {_dataType = TypeFun [TypeInt, TypeFun [TypeReal, TypeBool], TypeDef "Ha"], _value = Nothing}), ("Haha", Value {_dataType = TypeFun [TypeDef "Ha"], _value = Nothing})]], _definitions = [fromList [("Ha", DataDef "Ha" [MultDef {multName = "Haha", parameters = [TypeDef "Ha"]}, MultDef {multName = "Ha", parameters = [TypeInt, TypeFun [TypeReal, TypeBool], TypeDef "Ha"]}])]], _tempRefs = ["temp2"]}
    expected' = Right AlexUserState {_values = [fromList [("Empty", Value {_dataType = TypeFun [TypeDef "List"], _value = Nothing}), ("List", Value {_dataType = TypeFun [TypeInt, TypeDef "List", TypeDef "List"], _value = Nothing})]], _definitions = [fromList [("List", DataDef "List" [MultDef {multName = "Empty", parameters = [TypeDef "List"]}, MultDef {multName = "List", parameters = [TypeInt, TypeDef "List", TypeDef "List"]}])]], _tempRefs = ["temp2"]}
    expected'' = Left "Happy error on line and column (1,13): name B is not defined at [fromList [(\"A\",DataDef \"A\" [])]]"
    alreadyDefined = Left "Happy error on line and column (1,27): Data name Ha is already defined at [fromList [(\"Ha\",DataDef \"Ha\" [MultDef {multName = \"Ha\", parameters = [TypeDef \"Ha\"]}])]]"

goldenTests = do
  codeFiles <- findByExtension [".chs"] "./files"
  return $ testGroup "Language Code to Three Adress Code Tests" 
    [ goldenVsString 
        (takeBaseName codeFile)
        tacFile
        (runExpression' <$> LBS.readFile codeFile ) 
    | codeFile <- codeFiles 
    , let tacFile = replaceExtension codeFile ".tac"
    ]

tests = testGroup "Tests" [dataTests, funcTests, goldenTests]

main = defaultMain tests
