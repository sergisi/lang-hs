-- |

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Parser
import           ParserData
import           AlexUserState
import           Data.Map.Strict as Map
import           Control.Monad                  ( when
                                                , liftM
                                                , liftM2
                                                )

dataTests = testGroup "Data tests"
  [ testCase "Normal test" $ expected @=? runExpression' "data Ha = Ha Int (Real -> Bool) | Haha;"
  , testCase "List test" $ expected' @=? runExpression' "data List = List Int List | Empty;"
  , testCase "Name not defined" $ expected'' @=? runExpression' "data A = A B"
  , testCase "Name already defined" $ alreadyDefined @=? runExpression' "data Ha = Ha;data Ha = Ha;"
  ]
  where
    expected = Right (AlexUserState {_values = [fromList [("Ha",Value {_dataType = TypeFun [TypeInt,TypeFun [TypeReal,TypeBool],TypeDef "Ha"], _value = Nothing}),("Haha",Value {_dataType = TypeFun [TypeDef "Ha"], _value = Nothing})]], _definitions = [fromList [("Ha",DataDef "Ha" [MultDef {multName = "Haha", parameters = [TypeDef "Ha"]},MultDef {multName = "Ha", parameters = [TypeInt,TypeFun [TypeReal,TypeBool],TypeDef "Ha"]}])]]})
    expected' = Right (AlexUserState {_values = [fromList [("Empty",Value {_dataType = TypeFun [TypeDef "List"], _value = Nothing}),("List",Value {_dataType = TypeFun [TypeInt,TypeDef "List",TypeDef "List"], _value = Nothing})]], _definitions = [fromList [("List",DataDef "List" [MultDef {multName = "Empty", parameters = [TypeDef "List"]},MultDef {multName = "List", parameters = [TypeInt,TypeDef "List",TypeDef "List"]}])]]})
    expected'' = Left "Happy error on line and column (1,13): name B is not defined at [fromList [(\"A\",DataDef \"A\" [])]]"
    alreadyDefined = Left "Happy error on line and column (1,27): Data name Ha is already defined at [fromList [(\"Ha\",DataDef \"Ha\" [MultDef {multName = \"Ha\", parameters = [TypeDef \"Ha\"]}])]]"

tests = testGroup "Tests" [dataTests]

main = defaultMain tests
