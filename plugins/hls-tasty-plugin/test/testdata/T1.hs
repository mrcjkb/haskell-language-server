module T1 where

import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "testGroupT1"
    [ testCase "testCase1" undefined
    ]
