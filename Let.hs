-- Perfil em Eng. de Sw
-- Aula de 17/2/2023

module Let where
import Parser
import Data.Char
import Exp
import Prelude hiding ((<*>),(<$>))

-- Syntax Abstracta da Ling. Let
data Let = Let Items Exp
        deriving Show

type Items = [Item]

data Item = NestedLet String Let
          | Decl      String Exp
          deriving Show
  
-- Combinator Parser
pLet = f <$> token' "let" <*> symbol' '{' <*> pItems 
                          <*> symbol' '}' <*> token' "in"
                          <*> pExp
       where f l a i f _ e = Let i e 

pItems =         succeed []
      <|>  f <$> pItem <*> symbol' ';' <*> pItems
    where f a b c = a:c

pItem =  f <$>  ident <*> symbol' '=' <*> pExp
     <|> g <$>  ident <*> symbol' '=' <*> pLet
     where  f a b c = Decl a c
            g a b c = NestedLet a c

pId =  f <$>  satisfy' (isLower)
   <|> g <$>   satisfy' isLower <*> pId
   where f a = [a]
         g a b = a : b

pInt =  f <$>   satisfy' isDigit
    <|> g <$>   satisfy' isDigit <*> pInt
    where f a = [a]
          g a b = a:b
