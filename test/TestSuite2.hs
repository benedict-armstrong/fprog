module TestSuite2 where

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe2 Tests"
    [ testCase "Trivialer Test" $
        1 @?= 1
    ]