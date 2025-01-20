module Task2Suite where

import Test.Tasty
import Test.Tasty.QuickCheck

import Task2 (luhnDec, luhnHex, luhnModN, validateDec, validateHex)
import Data.Char (digitToInt, intToDigit)


task2Tests :: TestTree
task2Tests = testGroup "Task2"
  [ testProperty "luhnDec" $
      withMaxSuccess 1000 $ forAllBlind (elements decCases) $
        \(x, res) -> 
          let digits = map digitToInt (show x)
          in  counterexample ("luhnDec " ++ show digits) $ luhnDec digits === res

  , testProperty "validateDec" $
      withMaxSuccess 1000 $ forAllBlind (elements decCases) $
        \(x, res) -> conjoin
          [ counterexample ("validateDec " ++ show x) $ validateDec x === False
          , let x' = x * 10 + fromIntegral res
            in counterexample ("validateDec " ++ show x') $ validateDec x' === True
          ]

  , testProperty "luhnHex" $
      withMaxSuccess 1000 $ forAllBlind (elements hexCases) $
        \(x, res) -> 
          counterexample ("luhnHex " ++ show x) $ luhnHex x === res

  , testProperty "validateHex" $
      withMaxSuccess 1000 $ forAllBlind (elements hexCases) $
        \(x, res) -> conjoin
          [ counterexample ("validateHex " ++ show x) $ validateHex x === False
          , let x' = x ++ [intToDigit res]
            in counterexample ("validateHex " ++ show x') $ validateHex x' === True
          ]

  , testProperty "luhnModN ['a'..'z']" $
      withMaxSuccess 1000 $ forAllBlind (elements alphCases) $
        \(x, res) -> 
          counterexample ("luhnModN " ++ show alphSize ++ " (\\c -> fromEnum c - fromEnum 'a') " ++ show x) $
            luhnModN alphSize alphToInt x === res
  ]

------------------------
-- Test cases
------------------------

decCases :: [(Integer, Int)]
decCases =
    [ (i, fromIntegral (i * 2) `norm` 10 `modComp` 10) | i <- [1..9] ]
 ++ [ (123, 0)
    , (3456, 1)
    , (401288888888188, 1)
    ] 

hexCases :: [(String, Int)]
hexCases =
    [ ([c], (digitToInt c * 2) `norm` 16 `modComp` 16) | c <- ['1'..'9'] ++ ['a'..'f'] ++ ['A'..'F'] ]
 ++ [ ("123", 6)
    , ("3456", 4)
    , ("401288888888188", 5)
    , ("123abc", digitToInt 'f')
    ] 

alphCases :: [(String, Int)]
alphCases =
    [ ([c], (alphToInt c * 2) `norm` alphSize `modComp` alphSize) | c <- ['b'..'z'] ]
 ++ [ ("abc", alphToInt 'u')
    , ("hello", alphToInt 'u')
    , ("haskell", alphToInt 'x')
    ] 

------------------------
-- Utilities
------------------------

alphSize :: Int
alphSize = fromEnum 'z' - fromEnum 'a'

alphToInt :: Char -> Int
alphToInt c = fromEnum c - fromEnum 'a'

-- >>> intToAlph 20
-- 'u'
-- >>> intToAlph 23
-- 'x'

intToAlph :: Int -> Char
intToAlph i = toEnum (i + fromEnum 'a')

norm :: Int -> Int -> Int
norm x n
  | x >= n    = x - (n - 1)
  | otherwise = x

modComp :: Int -> Int -> Int
modComp x n = (n - (x `mod` n)) `mod` n
