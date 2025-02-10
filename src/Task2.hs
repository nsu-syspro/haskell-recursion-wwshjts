{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- The above pragma enables all warnings
-- (except for unused imports from Task1)
module Task2 where

-- Explicit import of Prelude to hide functions
-- that are not supposed to be used in this assignment
import Prelude hiding (reverse, map, filter, sum, foldl, foldr, length, head, tail, init, last, show, read)

-- You can reuse already implemented functions from Task1
-- by listing them in this import clause
-- NOTE: only listed functions are imported, everything else remains hidden
import Task1 (reverse, map, sum, doubleEveryOtherL, toDigits, dropLast, last)

-----------------------------------
--
-- Computes check digit number for given abstract characters using Luhn algorithm mod N
-- and given mapping function
--
-- Usage example:
--
-- >>> luhnModN 10 id [3,4,5,6]
-- 1

luhnModN :: Int -> (a -> Int) -> [a] -> Int
luhnModN base f list = 
    luhnFormulaModN base (sum (map (normalizeModN base) (doubleEveryOtherL (map f list))))

luhnFormulaModN :: Int -> Int -> Int
luhnFormulaModN n s = (n - (s `mod` n)) `mod` n 

-----------------------------------
--
-- Normalizes number as described in luhnModN algorithm 
--
-- Usage example:
--
-- >>> normalizeModN 16 30 
-- 15 

normalizeModN :: Int -> Int -> Int
normalizeModN base n
    | n >= base    = n - base + 1  
    | otherwise = n 

-----------------------------------
--
-- Computes decimal check digit for given digits using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> luhnDec [3,4,5,6]
-- 1

luhnDec :: [Int] -> Int
luhnDec = luhnModN 10 id

-----------------------------------
--
-- Computes hexadecimal check digit number for given digits using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> luhnHex "123abc"
-- 15

luhnHex :: [Char] -> Int
luhnHex = luhnModN 16 digitToInt

-----------------------------------
--
-- Converts given hexadecimal digit to its ordinal number between 0 and 15
--
-- Usage example:
--
-- >>> map digitToInt ['0'..'9']
-- [0,1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

digitToInt :: Char -> Int
digitToInt char
    | char `elem` ['0'..'9'] = fromEnum char - fromEnum '0'
    | char `elem` ['a'..'f'] = fromEnum char - fromEnum 'a' + 10
    | char `elem` ['A'..'F'] = fromEnum char - fromEnum 'A' + 10
    | otherwise              = error "Can't apply function not ot hexadecimal digit"

-----------------------------------
--
-- Inversion of digitToInt 
--
-- Usage example:
--
-- >>> map digitToInt [0..9]
-- ['0',1,2,3,4,5,6,7,8,9]
-- >>> map digitToInt ['a'..'f']
-- [10,11,12,13,14,15]
-- >>> map digitToInt ['A'..'F']
-- [10,11,12,13,14,15]

intToDigit :: Int -> Char 
intToDigit n 
    | n `elem` [0..9]   = toEnum (n + fromEnum '0')
    | n `elem` [10..15] = toEnum (n + fromEnum 'a' - digitToInt 'a')
    | otherwise         = error "Can't apply function not ot hexadecimal digit"

-----------------------------------
--
-- Checks whether the last decimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 10
--
-- Usage example:
--
-- >>> validateDec 3456
-- False
-- >>> validateDec 34561
-- True
-- >>> validateDec 34562
-- False

validateDec :: Integer -> Bool
validateDec n = validatePoly 10 (toDigits n) id  -- `mod` and `div` are still effective than dropLast(

-----------------------------------
--
-- Checks whether the last hexadecimal digit is a valid check digit
-- for the rest of the given number using Luhn algorithm mod 16
--
-- Usage example:
--
-- >>> validateHex "123abc"
-- False
-- >>> validateHex "123abcf"
-- True
-- >>> validateHex "123abc0"
-- False

validateHex :: [Char] -> Bool
validateHex hexD = validatePoly 16 hexD digitToInt 


-----------------------------------
--
-- Polymorphic validate function 
-- first argument - is the base of radix
-- second - list of number digits
-- third  - bijection between symbols of redix notation and their ordinal numbers 
--

validatePoly :: Int -> [a] -> (a -> Int) -> Bool
validatePoly base digits order =
    luhnModN base order (dropLast digits) == order (last digits)
