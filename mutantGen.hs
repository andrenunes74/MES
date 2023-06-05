{-# LANGUAGE DeriveDataTypeable #-}
module MutantGen where
import Core
import Reverse
import Generator
import Opt
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Test.QuickCheck hiding (Args)

treeT1 = [(OpenFuncao (DefFuncao (Name "mht") [Arg (Var "qhpzs")] [(Decl ("x") (Add (Const 2) (Var "qhpzs"))),(NestedReturn (Return (Var "x")))]),"")]
treeT2 = [(OpenFuncao (DefFuncao (Name "mht") [Arg (Var "qhpzs")] [(Decl ("x") (Mul (Const 2) (Var "qhpzs"))),(NestedReturn (Return (Var "x")))]),"")]
treeT3 = [(OpenFuncao (DefFuncao (Name "mht") [Arg (Var "qhpzs")] [(Decl ("x") (Sub (Const 2) (Var "qhpzs"))),(NestedReturn (Return (Var "x")))]),"")]
treeT4 = [(OpenFuncao (DefFuncao (Name "mht") [Arg (Var "qhpzs")] [(Decl ("x") (Div (Const 2) (Var "qhpzs"))),(NestedReturn (Return (Var "x")))]),"")]
treeT5 = [(OpenFuncao (DefFuncao (Name "mht") [Arg (Var "qhpzs")] [(Decl ("x") (Div (Const 2) (Var "qhpzs"))),(Decl ("x") (Add (Const 2) (Var "qhpzs"))),(Decl ("x") (Mul (Const 2) (Var "qhpzs"))),(Decl ("x") (Sub (Const 2) (Var "qhpzs"))),(NestedReturn (Return (Var "x")))]),"")]

{---------------------------------------------------------------------------------------
********************************** Mutant Generator ************************************
----------------------------------------------------------------------------------------}

--Mutant that turns addition into subtraction
m1 :: Item -> Maybe Item
m1 (Decl a (Add b c)) = Just (Decl a (Sub b c))
m1 x = Just x

--Mutant that turns multiplication into division
m2 :: Item -> Maybe Item
m2 (Decl a (Mul b c)) = Just (Decl a (Div b c))
m2 x = Just x

--Mutant that swaps subtraction elements
m3 :: Item -> Maybe Item
m3 (Decl a (Sub b c)) = Just (Decl a (Sub c b))
m3 x = Just x

--Mutant that swaps division elements
m4 :: Item -> Maybe Item
m4 (Decl a (Div b c)) = Just (Decl a (Div c b))
m4 x = Just x

--Mutant that swaps less operator elements
m5 :: Item -> Maybe Item
m5 (OpenIf (If (Less a b) c)) = Just (OpenIf (If (Less b a) c))
m5 (NestedIf (If (Less a b) c)) = Just (OpenIf (If (Less b a) c))
m5 x = Just x

--Function that aplies one given mutant with full_tdTP strategie
mutant f lista =
    let listaZipper = toZipper lista
        Just listaNova = applyTP (full_tdTP step) listaZipper
            where step = idTP `adhocTP` f
    in 
    fromZipper listaNova

--Function that aplies a random mutant with full_tdTP strategie
applyRandomMutant :: [(Item, String)] -> IO [(Item, String)]
applyRandomMutant tree = do
  randomIndex <- generate (choose (1, 5) :: Gen Int)
  case randomIndex of
    1 -> return (mutant m1 tree)
    2 -> return (mutant m2 tree)
    3 -> return (mutant m3 tree)
    4 -> return (mutant m4 tree)
    5 -> return (mutant m5 tree)
    _ -> error "Invalid mutant index"
