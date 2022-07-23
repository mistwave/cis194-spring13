
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


  -- exercise 5
  type Peg = String
  type Move = (Peg, Peg)

  -- move from the first peg to the *second*
  hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
  hanoi 0 _ _ _ = []
  hanoi 1 a b _ = [(a, b)]
  hanoi n a b c =
    let step1 = hanoi (n-1) a c b
        step2 = hanoi 1 a b "unused"
        step3 = hanoi (n-1) c b a
    in step1 ++ step2 ++ step3

  -- exercise 6
  -- move from the first peg to the *second*
  hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
  hanoi4 0 _ _ _ _ = []
  hanoi4 1 a b _ _ = [(a, b)]
  hanoi4 2 a b c _ = hanoi 2 a b c
  hanoi4 n a b c d =
    let pile1 = n `div` 2
        pile2 = n - pile1
        step1 = hanoi4 pile1 a c b d
        step2 = hanoi pile2 a b d
        step3 = hanoi4 pile1 c b a d
        in step1 ++ step2 ++ step3
