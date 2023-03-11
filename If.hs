{-# LANGUAGE DeriveDataTypeable #-}
module If where
import Parser
import Data.Char
import Exp
import Let
import Prelude hiding ((<*>),(<$>))

data If = If Exp ItemsF
        | Else ItemsF
        deriving Show

type ItemsF = [ItemF]

data ItemF = NestedIf    String If
           | DeclIf      String Exp
           deriving Show

pIf = f <$> token' "if" <*> symbol' '(' <*> pExp 
                        <*> symbol' ')'
                        <*> symbol' '{'
                        <*> pItemsIf
                        <*> symbol' '}'
      <|> g <$> token' "if" <*> symbol' '(' <*> pExp 
                        <*> symbol' ')'
                        <*> symbol' '{'
                        <*> pItemsIf
                        <*> symbol' '}'
                        <*> token' "else"
                        <*> symbol' '{'
                        <*> pItemsIf
                        <*> symbol' '}'
        where f _ _ a _ _ b _ = If a b
              g _ _ a _ _ b _ _ _ c _ = Else c                       

pItemsIf =         succeed []
      <|>  f <$> pItemIf <*> symbol' ';' <*> pItemsIf
      where f a b c = a:c

pItemIf =  f <$>  ident <*> symbol' '=' <*> pExp
     <|> g <$>  ident <*> symbol' '=' <*> pIf
     where  f a b c = DeclIf a c
            g a b c = NestedIf a c




