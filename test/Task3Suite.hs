{-# LANGUAGE ImportQualifiedPost #-}

module Task3Suite (task3Tests) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Task3 (hanoi, Move, Peg)
import Data.List (nub, sort, (\\), intercalate)
import Data.Map (Map, (!))
import Data.Map qualified as M
import Control.Monad

task3Tests :: TestTree
task3Tests = testGroup "Task3"
  [ testGroup "hanoi"
    [ testProperty "Valid pegs" $
        forAllInputs 
          [ ("a",  "b",  "c")
          , ("A",  "B",  "C")
          , ("aa", "bb", "cc")
          ]
          prop_validPegs
    , testProperty "Valid moves" $
        forAllInputs 
          [ ("a",  "b",  "c") ]
          prop_validMoves
    ]
  ]

maxSize :: Int
maxSize = 15

prop_validPegs :: HanoiProp
prop_validPegs _ a b c moves =
  counterexample ("Unexpected pegs " ++ show unexpectedPegs ++ " in " ++ show moves) $ null unexpectedPegs
 where
  expectedPegs = sort [a,b,c]
  actualPegs = sort $ nub $ concatMap (\(x, y) -> [x,y]) moves
  unexpectedPegs = actualPegs \\ expectedPegs

prop_validMoves :: HanoiProp
prop_validMoves n a b c moves =
  counterexample message $ actual == Right expected
 where
  initial  = initState n a b c
  expected = initState n b a c
  actual   = simulate moves initial
  message  = either id report actual ++ "\n" ++ "Moves: " ++ show moves
  report s = unlines 
    [ "Unexpected end state"
    , "Expected:"
    , prettyState expected
    , "Actual:"
    , prettyState s
    ]

type HanoiTower = [Int]
type HanoiState = Map Peg HanoiTower

initState :: Int -> Peg -> Peg -> Peg -> HanoiState
initState n a b c = M.fromList [(a,[1..n]), (b,[]), (c,[])]

-- >>> prettyState $ initState 3 "a" "b" "c"
-- "  a[1,2,3]\n  b[]\n  c[]"

prettyState :: HanoiState -> String
prettyState s = intercalate "\n" $ map (\(p,vs) -> "  " ++ p ++ ": " ++ show vs) $ M.toList s

simulate :: [Move] -> HanoiState -> Either String HanoiState
simulate ms s = foldM simulate1 s ms

simulate1 :: HanoiState -> Move -> Either String HanoiState
simulate1 s move@(a,b)
  | M.notMember a s || M.notMember b s = Left $ "Unexpected pegs in " ++ show move
  | otherwise = case (s ! a, s ! b) of
    ([], _)        -> Left $ "Empty peg " ++ a ++ " at move " ++ show move
    (x : xs, [])   -> Right $ M.insert a xs $ M.insert b [x] s
    (x : xs, y : ys)
      | x < y      -> Right $ M.insert a xs $ M.insert b (x : y : ys) s
      | otherwise  -> Left $ "Invalid move " ++ show move ++ " with state " ++ show s
  

-------------------------

type HanoiProp = Int -> Peg -> Peg -> Peg -> [Move] -> Property

forAllInputs :: [(Peg, Peg, Peg)] -> HanoiProp -> Property
forAllInputs pegs f =
  forAllShrink (chooseInt (1, maxSize)) (filter (> 0) . shrinkIntegral)
    (\n -> forAll (elements pegs) $ \(a,b,c) -> f n a b c (hanoi n a b c))
