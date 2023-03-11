{-# LANGUAGE DeriveDataTypeable #-}
module While where
import Parser
import Data.Char
import Exp
import Let
import Prelude hiding ((<*>),(<$>))

data While = While Exp ItemsW
        deriving Show

type ItemsW = [ItemW]

data ItemW = NestedWhile String While
          | DeclW        String Exp
          deriving Show

pWhile = f <$> token' "while" <*> symbol' '(' <*> pExp 
                              <*> symbol' ')'
                              <*> symbol' '{' <*> pItemsW
                              <*> symbol' '}'
       where f l b a c i g x = While a g 

pItemsW =         succeed []
      <|>  f <$> pItemW <*> symbol' ';' <*> pItemsW
    where f a b c = a:c

pItemW =  f <$>  ident <*> symbol' '=' <*> pExp
     <|> g <$>  ident <*> symbol' '=' <*> pWhile
     where  f a b c = DeclW a c
            g a b c = NestedWhile a c