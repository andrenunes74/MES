{-# LANGUAGE DeriveDataTypeable #-}
module Funcao where
import Parser
import Data.Char
import Exp
import Let
import Opt
import Prelude hiding ((<*>),(<$>))

data Funcao = Funcao Name Args
            deriving Show

data Name = Name String 
          deriving Show

data Arg = Arg Exp
         | NestedFuncao Funcao
         deriving Show

type Args = [Arg] 

pFuncao = f <$> token' "def" <*> pName <*> symbol' '(' 
                             <*> pArgs
                             <*> symbol' ')'
         <|> g <$> pName <*> symbol' '(' 
                             <*> pArgs
                             <*> symbol' ')'
       where f _ b _ a _ = Funcao b a
             g b _ a _ = Funcao b a

pName = f <$> ident
        where f a = Name a

pArgs =         succeed []
      <|>  f <$> pArg <*> symbol' ',' <*> pArgs
      <|>  g <$> pArg
    where f a _ c = a:c
          g a = [a]

pArg =  f <$>  pExp
    <|> g <$> pFuncao
     where  f a = Arg (optExp a)
            g a = NestedFuncao a
