module Task1Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Task1 (luhn, validate)
import Task2Suite (decCases)
import Data.Char (digitToInt)

-- See decCases in Task2Suite

task1Tests :: TestTree
task1Tests = testGroup "Task1"
  [ testProperty "luhn" $
      withMaxSuccess 1000 $ forAllBlind (elements decCases) $
        \(x, res) -> 
          let digits = map digitToInt (show x)
          in  counterexample ("luhn " ++ show digits) $ luhn digits === res

  , testProperty "validate" $
      withMaxSuccess 1000 $ forAllBlind (elements decCases) $
        \(x, res) -> conjoin
          [ counterexample ("validate " ++ show x) $ validate x === False
          , let x' = x * 10 + fromIntegral res
            in counterexample ("validate " ++ show x') $ validate x' === True
          ]
  ]
