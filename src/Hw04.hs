module Hw04 where

import Data.List (foldl')



-- exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = product $ map (\x -> x-2) $ filter even xs


-- fun2 0 will loop infinitely
fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' n = sum $ filter even $ takeWhile (>1) $ iterate (\x -> if even x then x `div` 2 else 3*x + 1) n


-- exercise2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

getDepth :: Tree a -> Tree a -> Integer
getDepth _ Leaf = 0
getDepth Leaf _ = 0
getDepth (Node ld _ _ _) (Node rd _ _ _) = max ld rd

insert :: a -> Tree a -> Tree a
insert aVal Leaf = Node 0 Leaf aVal Leaf
insert aVal (Node d Leaf v Leaf) = Node (d+1) (insert aVal Leaf) v Leaf
insert aVal (Node d left@(Node _ _ _ _) v Leaf) = Node d left v (insert aVal Leaf)
insert aVal (Node d Leaf v right@(Node _ _ _ _)) = Node d (insert aVal Leaf) v right
insert aVal (Node _ left@(Node ld _ _ _) v right@(Node rd _ _ _))
  | ld <= rd = Node newDepth ltree v right
  | otherwise = Node newDepth left v rtree
    where ltree = insert aVal left
          rtree = insert aVal right
          newDepth = 1 + getDepth ltree rtree

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree as = foldl' (\acc a -> insert a acc) Leaf as

example2 :: Tree Char
example2 = foldTree "ABCDEFGHIJ"


-- exercise 3
xor :: [Bool] -> Bool
xor = foldr (\a acc -> if a then not acc else acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a bs -> f a : bs) []

myReverse :: [a] -> [a]
myReverse [] = []
myReverse as = helper as []
  where
    helper [] acc = acc
    helper (x:xs) acc = helper xs (x: acc)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl f base xs = foldr (\a acc -> f acc a) base (myReverse xs)


-- exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2 : (map (\x -> 2 * x + 1) $ filter (`notElem` notPrimes) [1..n])
  where 
    f (x,y) = x + y + 2 * x * y 
    notPrimes = filter (<= n) $ map f $ [(x, y) | x <- [1..n], y <- [1..n], x <= y]