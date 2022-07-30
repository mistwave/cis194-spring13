module Hw03 where

-- exercise 1
skip :: Int -> [a] -> [a]
skip n xs = case drop n xs of
  [] -> []
  (a:as) -> a : skip n as

skips :: [a] -> [[a]]
skips s = [skip n s | n <- [0..length s - 1]]

-- exercise 2
-- f checks if the 2nd element of a list is local maximum
f :: [Integer] -> [Integer]
f (a:b:c:_)
  | b > a && b > c = [b]
f _ = []

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima p@(_:xs) = f p ++ localMaxima xs

-- exercise3
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

counter :: [Int] -> [Int]
counter xs = [count (==n) xs | n <- [0..9]]

stars :: [Int] -> [String]
stars xs = map (\n -> replicate n '*' ++ replicate (maxLen-n) ' ') xs
        where maxLen = maximum xs

rotate :: [String] -> [String]
rotate xs = map (\n -> [s !! n | s <- xs]) (reverse [0..length (head xs) - 1])

join :: String -> [String] -> String
join d = foldr (\x acc -> x ++ d ++ acc) ""

histogram :: [Integer] -> String
histogram xs = join "\n" (rotate (stars (counter (map fromInteger xs)))) ++ "==========\n0123456789\n"
