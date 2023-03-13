{-# LANGUAGE DeriveDataTypeable #-}
module Exp where
import Prelude hiding ((<*>),(<$>))
import Parser

data Exp = Add Exp Exp
         | Sub Exp Exp
         | Div Exp Exp
         | Mul Exp Exp
         | Less Exp Exp
         | Greater Exp Exp
         | Equals Exp Exp
         | GTE Exp Exp
         | LTE Exp Exp
         | And Exp Exp
         | Or Exp Exp
         | Not Exp
         | Const Int
         | Var String
         deriving Show

pExp :: Parser Exp
pExp = a  <$> pTermo <*> symbol' '+' <*> symbol' '+'
    <|> b  <$> pTermo <*> symbol' '-' <*> symbol' '-'
    <|> f  <$> pTermo <*> symbol' '+' <*> pExp
    <|> id <$> pTermo
    <|> g  <$> pTermo <*> symbol' '-' <*> pExp
    <|> x  <$> pTermo <*> symbol' '>' <*> pExp
    <|> y  <$> pTermo <*> symbol' '<' <*> pExp
    <|> z  <$> pTermo <*> symbol' '=' <*> symbol' '=' <*> pExp
    <|> t  <$> pTermo <*> symbol' '&' <*> symbol' '&' <*> pExp
    <|> v  <$> pTermo <*> symbol' '|' <*> symbol' '|' <*> pExp
    <|> h  <$> pTermo <*> symbol' '!' <*> symbol' '=' <*> pExp
    <|> l  <$> pTermo <*> symbol' '>' <*> symbol' '=' <*> pExp
    <|> k  <$> pTermo <*> symbol' '<' <*> symbol' '=' <*> pExp
    where f a _ c = Add a c
          g a _ c = Sub a c
          x a _ c = Greater a c
          y a _ c = Less a c
          z a _ _ c = Equals a c
          t a _ _ c = And a c
          v a _ _ c = Or a c
          h a _ _ c = Not c
          l a _ _ c = GTE a c
          k a _ _ c = LTE a c
          a x _ _ = Add x (Const 1)
          b x _ _ = Sub x (Const 1)
    
pTermo :: Parser Exp
pTermo =  f  <$> pFactor <*> symbol' '*' <*> pTermo
      <|> id <$> pFactor
      <|> g  <$> pFactor <*> symbol' '/' <*> pTermo
    where f a _ c = Mul a c
          g a _ c = Div a c

pFactor :: Parser Exp
pFactor =  f   <$> number
       <|> Var <$> ident
       <|> g   <$> enclosedBy (symbol' '(')
                              pExp
                              (symbol' ')')
       <|> j   <$> enclosedBy (symbol' '(')
                              pTermo
                              (symbol' ')')
       where f a = Const (read a)
             g a = a
             j a = a