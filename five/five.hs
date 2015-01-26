{-# LANGUAGE FlexibleInstances #-}

module HW5 where

import ExprT
import Parser
import qualified Data.Map as M
import Control.Applicative (liftA2)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2


evalStr :: String -> Maybe Integer
evalStr = (fmap eval) . parseExp Lit Add Mul


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


newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit i = i > 0
  add = (||)
  mul = (&&)

instance Expr MinMax where
  lit = MinMax
  add (MinMax i) (MinMax j) = MinMax $ max i j
  mul (MinMax i) (MinMax j) = MinMax $ min i j

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 i) (Mod7 j) = Mod7 $ mod (i+j) 7
  mul (Mod7 i) (Mod7 j) = Mod7 $ mod (i*j) 7


class HasVars a where
  var :: String -> a


data VarExprT = VLit Integer
           | Var String
           | VAdd VarExprT VarExprT
           | VMul VarExprT VarExprT
           deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = \_ -> Just x
  add f g m = liftA2 (+) (f m) (g m)
  mul f g m = liftA2 (*) (f m) (g m)


withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
