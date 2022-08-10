module Hw05 where

import ExprT
import Parser

-- exercise 1
eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul


-- exercise 3
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

reify :: ExprT -> ExprT
reify = id

reifyExample :: ExprT
reifyExample = reify $ mul (add (lit 2) (lit 3)) (lit 4)


-- exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)
  
instance Expr Bool where
  lit x = x > 0
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Show, Eq)

instance Expr MinMax where 
  lit = MinMax
  add (MinMax x) (MinMax y) = lit (max x y)
  mul (MinMax x) (MinMax y) = lit (min x y)
  
newtype Mod7 = Mod7 Integer deriving (Show, Eq)
instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = lit (x+y)
  mul (Mod7 x) (Mod7 y) = lit (x*y)
  
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"


-- exercise 5


-- exercise 6
-- see Hw05VarExprT.hs
