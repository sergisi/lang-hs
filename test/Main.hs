-- |

module Main where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Parser
import           ParserData

import           Control.Monad                  ( when
                                                , liftM
                                                , liftM2
                                                )

dataTests = testGroup "Data tests"
  [ testCase "Normal test" $ expected @=? runExpression "data Ha = Ha Int (Double -> Bool) | Haha;"
  , testCase "List test" $ expected' @=? runExpression "data List = List Int List | Empty;"
  , testCase "Name not defined" $ expected'' @=? runExpression "data A = A B"
  ]
  where
    expected = Right [DataStatement (DataDef "Ha" [MultDef {multName = "Haha", parameters = []},MultDef {multName = "Ha", parameters = [TypeInt,TypeFun [TypeDef "Double",TypeBool]]}])]
    expected' = Right [DataStatement (DataDef "List" [MultDef {multName = "Empty", parameters = []},MultDef {multName = "List", parameters = [TypeInt,TypeDef "List"]}])]
    expected'' = Left "error"

tests = testGroup "Tests" [dataTests]

main = defaultMain tests
