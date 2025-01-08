{-# OPTIONS_GHC -Wall #-}
-- Note: the above pragma enables all warnings

module Task3 where

-----------------------
-- Helper type synonyms

type Peg = String
type Move = (Peg, Peg)

-----------------------

-- Usage examples
--
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]

hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi = error "TODO: define hanoi"

