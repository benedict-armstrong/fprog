module TestSuite1 where

import Test.Tasty
import Test.Tasty.HUnit

spec :: TestTree
spec =
  testGroup
    "Angabe1 Tests"
    [ testCase "Trivialer Test" $
        1 @?= 1,
      testCase "Noch ein Test" $
        (1 + 2) @?= 3
    ]