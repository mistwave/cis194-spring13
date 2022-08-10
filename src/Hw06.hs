{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Hw06 where


-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- exercise 2
fib2 :: Integer -> Integer
fib2 n = iter n 0 1
  where iter 0 res _ = res
        iter n a b = iter (n-1) (a + b) a

fibs2 :: [Integer]
fibs2 = map fib2 [0..]

-- exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show xs = "Stream:" ++ (show $ take 20 $ streamToList xs) ++ "..."

-- exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f x')
                      where x' = f x

-- exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = iter 0
  where iter n = interleaveStreams (streamRepeat n) (iter (n+1))


-- exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger x = Cons x (streamRepeat 0)
  negate (Cons x xs) = Cons (-x) (negate xs)
  (Cons x xs) + (Cons y ys) = Cons (x + y) (xs + ys)
  (Cons a0 a') * b@(Cons b0 b') = Cons (a0 * b0) (streamMap (*a0) b' + a' * b)


instance Fractional (Stream Integer) where
  (Cons a0 a') / (Cons b0 b') = q
    where q = Cons (a0 `div` b0) (streamMap (`div` b0) (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)


-- exercise 7
-- Mat a b c d stands for
-- [ a b
--   c d ]

data Matrix = Mat Integer Integer Integer Integer deriving(Show)

instance Num Matrix where
  (Mat a b c d) * (Mat h i j k) = Mat p q m n
    where p = a * h + b * j
          q = a * i + b * k
          m = c * h + d * j
          n = c * i + d * k

fibSeed :: Matrix
fibSeed = Mat 1 1 1 0

fib4 :: Integer -> Integer
fib4 n = case fibSeed ^ n of
  Mat _ res _ _ -> res

fibs4 :: [Integer]
fibs4 = 0 : map fib4 [1..]

