
module Hw01 where

  import Data.List as List (reverse, length)


  -- exercise 1
  toDigits :: Integer -> [Integer]
  toDigits = List.reverse . toDigitsRev

  toDigitsRev :: Integer -> [Integer]
  toDigitsRev n
    | n <= 0 = []
    | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

  -- exercise 2
  doubleEveryOther :: [Integer] -> [Integer]
  doubleEveryOther [] = []
  doubleEveryOther (x:xs)
    | ((List.length xs) `mod` 2) == 1 = (2 * x) : doubleEveryOther xs
    | otherwise = x : doubleEveryOther xs

  -- exercise 3
  sumList :: [Integer] -> Integer
  sumList = foldl (+) 0

  sumDigits :: [Integer] -> Integer
  sumDigits [] = 0
  sumDigits xs = sumList (fmap (sumList . toDigits) xs)
  
  -- exercise 4
  validate :: Integer -> Bool
  validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0


