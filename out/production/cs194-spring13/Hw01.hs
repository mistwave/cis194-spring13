module Hw01 where
  
  import Data.List as List (reverse)
  
  toDigits :: Integer -> [Integer]
  toDigits = List.reverse . toDigitsRev
   
  toDigitsRev :: Integer -> [Integer]
  toDigitsRev n
    | n <= 0 = []
    | n <= 9 = [n]
    | otherwise = (n `mod` 10) :: otherwise (n `div` 10)