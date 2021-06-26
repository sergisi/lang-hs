-- |
module Main where

import AlexUserState
import Control.Monad
  ( liftM,
    liftM2,
    when,
  )
import Data.Map.Strict as Map
import Lexer
import Parser
import ParserData
import SyntaxAnalysis
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden (goldenVsString, findByExtension)
import System.FilePath (takeBaseName, replaceExtension)
import Data.String

funcTests :: TestTree
funcTests =
  testGroup
    "Creates functions accordingly"
    [ testCase "Constructor with 3 parameters" $
        expected @=? constructor
    ]
  where
    constructor = runAlex "" $ generateMultDef (MultDef "name" [TypeInt, TypeChar, TypeReal, TypeFun [TypeInt, TypeBool], TypeBool]) 0
    expected = Right [TacFuncLabel "name", TacCopy (RefInf (RefVar "temp0") $ RefConstInt 0) (RefConstInt 0), TacGetParam (RefInf (RefVar "temp0") $ RefConstInt 1) 0, TacGetParam (RefInf (RefVar "temp0") $ RefConstInt 3) 1, TacGetParam (RefInf (RefVar "temp0") $ RefConstInt 4) 2, TacGetParam (RefInf (RefVar "temp0") $ RefConstInt 7) 3, TacReturn (RefVar "temp0")]


dataTests =
  testGroup
    "Data tests"
    [ testCase "Normal test" $ expected @=? runExpression' "data Ha = Ha Int (Real -> Bool) | Haha;",
      testCase "List test" $ expected' @=? runExpression' "data List = List Int List | Empty;",
      testCase "Name not defined" $ expected'' @=? runExpression' "data A = A B",
      testCase "Name already defined" $ alreadyDefined @=? runExpression' "data Ha = Ha;data Ha = Ha;"
    ]
  where
    expected = Right AlexUserState {_values = [fromList [("Ha", TypeFun [TypeInt, TypeFun [TypeReal, TypeBool], TypeDef "Ha"]), ("Haha", TypeFun [TypeDef "Ha"])]], _definitions = [fromList [("Ha", DataDef "Ha" [MultDef {multName = "Haha", parameters = [TypeDef "Ha"]}, MultDef {multName = "Ha", parameters = [TypeInt, TypeFun [TypeReal, TypeBool], TypeDef "Ha"]}])]], _tempRefs = ["temp2"]}
    expected' = Right AlexUserState {_values = [fromList [("Empty", TypeFun [TypeDef "List"]), ("List", TypeFun [TypeInt, TypeDef "List", TypeDef "List"])]], _definitions = [fromList [("List", DataDef "List" [MultDef {multName = "Empty", parameters = [TypeDef "List"]}, MultDef {multName = "List", parameters = [TypeInt, TypeDef "List", TypeDef "List"]}])]], _tempRefs = ["temp2"]}
    expected'' = Left "Happy error on line and column (1,13): name B is not defined at [fromList [(\"A\",DataDef \"A\" [])]]"
    alreadyDefined = Left "Happy error on line and column (1,27): Data name Ha is already defined at [fromList [(\"Ha\",DataDef \"Ha\" [MultDef {multName = \"Ha\", parameters = [TypeDef \"Ha\"]}])]]"

goldenTests :: String -> String -> IO TestTree
goldenTests name folder = do
  codeFiles <- findByExtension [".chs"] folder
  return $ testGroup name
    [ goldenVsString 
        (takeBaseName codeFile)
        tacFile
        (fromString . show . runExpression <$> readFile codeFile) 
    | codeFile <- codeFiles 
    , let tacFile = replaceExtension codeFile ".tac"
    ] 
    

tests :: IO TestTree
tests = do 
  gTestsFunctions <- goldenTests "Golden Functions" "./test/files/functions"
  gTestsConditional <- goldenTests "Golden Conditionals" "./test/files/conditional"
  gTestsAmbits <- goldenTests "Golden Ambits" "./test/files/ambits_programa"
  gTestsExpresions <- goldenTests "Golden Expressions" "./test/files/expresions"
  gTestsArrays <- goldenTests "Golden Arrays" "./test/files/arrays"
  gTestsWhile <- goldenTests "Golden While" "./test/files/while"
  gTestsRepeatUntil <- goldenTests "Golden Repeat/Until" "./test/files/repeat_until"
  return $ testGroup "Tests" [gTestsFunctions, 
                              gTestsConditional,
                              gTestsAmbits, 
                              gTestsExpresions,
                              gTestsArrays,
                              gTestsWhile,
                              gTestsRepeatUntil,
                              funcTests, 
                              dataTests]

main = defaultMain =<< tests
