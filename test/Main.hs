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

testSimplify s exp = testCase s (exp @=? runExpression s)

integersTests = testGroup
  "Integers tests"
  [ testSimplify "((2))" $ Right [Right 2]
  , testSimplify "(2 * 3) + 2" $ Right [Right 8]
  , testSimplify "(2 * 3) div (4 div 2)" $ Right [Right 3]
  , testSimplify "(2 * 3) mod (4 div 2)" $ Right [Right 0]
  , testSimplify "2 * (3 + 2)" $ Right [Right 10]
  , testSimplify "" $ Right []
  , testSimplify "a = 2; b = 3; c = a + b; a*b"
    $ Right [Right 2, Right 3, Right 5, Right 6]
  ]

realTests = testGroup
  "Reals tests"
  [ testSimplify "3.2 + 4.3" $ Right [Left 7.5]
  , testSimplify "1.5 * 2.0" $ Right [Left 3.0]
  , testSimplify "4.5 / 2.25" $ Right [Left 2.0]
  ]

castTests = testGroup
  "Cast Tests"
  [testSimplify "a = int 3.2; B = real 3" $ Right [Right 3, Left 3]]

tests = testGroup "Tests" [integersTests, realTests, castTests]

main = defaultMain tests
