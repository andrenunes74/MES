{-# LANGUAGE DeriveDataTypeable #-}
module Opt where
import Let 
import Exp 

optLet :: Let -> Let
optLet (Let its exp) = Let (optIts its) (optExp exp)

optIts [] = []
optIts (h:t) = optIt h : optIts t

optIt (Decl a b) = Decl a (optExp b)
optIt (NestedLet a b) = NestedLet a (optLet b)

optExp :: Exp -> Exp
optExp (Add e (Const 0)) = (optExp e)
optExp (Add e1 e2) = Add (optExp e1) (optExp e2)
optExp (Sub e (Const 0)) = (optExp e)
optExp (Sub e1 e2) = Sub (optExp e1) (optExp e2)
optExp (Mul e (Const 0)) = (optExp (Const 0))
optExp (Mul e1 e2) = Mul (optExp e1) (optExp e2)
optExp (Mul e (Const 1)) = e
optExp (Div e (Const 0)) = (optExp (Const 0))
optExp (Div e1 e2) = Div (optExp e1) (optExp e2)
optExp (Div e (Const 1)) = e
optExp e = e