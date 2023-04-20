{-Module that contains the datatypes of the parser-}
module Core where
{--------------------------------
********** Expressions **********
---------------------------------}
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
         | Inc Exp
         | Dec Exp
         | Bug String
         deriving Show

{--------------------------------
************* Item **************
---------------------------------}
data Item = Decl String Exp
          | Arg Exp
          | Increment Exp
          | Decrement Exp
          | NestedIf If
          | OpenIf If
          | NestedWhile While
          | OpenWhile While
          | NestedLet String Let
          | OpenLet Let
          | NestedFuncao Funcao
          | OpenFuncao Funcao
          deriving Show

type Items = [Item]

{--------------------------------
************* Let ***************
---------------------------------}
data Let = Let Items Exp
        deriving Show

{--------------------------------
********** Functions ************
---------------------------------}
data Funcao = Funcao Name Args
            | DefFuncao Name Args Items
            deriving Show

data Name = Name String 
          deriving Show

type Args = [Item] 

{--------------------------------
************** If ***************
---------------------------------}
data If = If Exp Items
        | Else Items
        deriving Show
        
{--------------------------------
************ While **************
---------------------------------}
data While = While Exp Items
        deriving Show
