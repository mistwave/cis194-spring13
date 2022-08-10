{-# LANGUAGE FlexibleInstances #-}
module Hw05VarExprT where

import qualified Data.Map as M
import Hw05

-- exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var


instance Expr (M.Map String Integer -> Maybe Integer) where
  lit = const . Just
  add f g m = f m >>= (\x -> fmap (+x) (g m))
  mul f g m = f m >>= (\x -> fmap (*x) (g m))

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs