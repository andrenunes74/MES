{-# LANGUAGE DeriveDataTypeable #-}
module Opt where
import Exp 

{-
optLet :: Let -> Let
optLet (Let its exp) = Let (optIts its) (optExp exp)

optIts [] = []
optIts (h:t) = optIt h : optIts t

optIt (Decl a b) = Decl a (optExp b)
optIt (NestedLet a b) = NestedLet a (optLet b)
-}

optExp :: Exp -> Exp
optExp (Add e (Const 0)) = (optExp e)
optExp (Add e1 e2) = Add (optExp e1) (optExp e2)
optExp (Sub e (Const 0)) = (optExp e)
optExp (Sub e1 e2) = Sub (optExp e1) (optExp e2)
optExp (Mul e (Const 0)) = (optExp (Const 0))
optExp (Mul e (Const 1)) = (optExp e)
optExp (Mul e1 e2) = Mul (optExp e1) (optExp e2)
optExp (Div e (Const 1)) = (optExp e)
optExp (Div e (Const 0)) = (optExp (Const 0))
optExp (Div e1 e2) = Div (optExp e1) (optExp e2)
optExp (Equals e1 e2) = Equals (optExp e1) (optExp e2)
optExp (Less e1 e2) = Less (optExp e1) (optExp e2)
optExp (Greater e1 e2) = Greater (optExp e1) (optExp e2)
optExp (GTE e1 e2) = GTE (optExp e1) (optExp e2)
optExp (LTE e1 e2) = LTE (optExp e1) (optExp e2)
optExp (And e1 e2) = And (optExp e1) (optExp e2)
optExp (Or e1 e2) = Or (optExp e1) (optExp e2)
optExp (Not e1) = Not (optExp e1)
optExp e = e