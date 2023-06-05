module Generator where
import Core
import Reverse
import Test.QuickCheck hiding (Args)

{--------------------------------------------------------------------------------------------------------
******************************************* Language Generator ******************************************
---------------------------------------------------------------------------------------------------------}

instance Arbitrary Exp where
    arbitrary = sized genExp
genExp s = frequency [(1,do f <- arbitrary
                            return (Const f)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l) && not(isLogic l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r) && not(isLogic r))
                               return (Add l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l) && not(isLogic l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r) && not(isLogic r))
                               return (Sub l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l) && not(isLogic l)) 
                               r <- genExp s' `suchThat` (\r -> not(isReturn r) && not(isLogic r) && not(isZero r))
                               return (Div l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l) && not(isLogic l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r) && not(isLogic r))
                               return (Mul l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r))
                               return (Less l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r))
                               return (Greater l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r))
                               return (Equals l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r))
                               return (GTE l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r))
                               return (LTE l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r))
                               return (And l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l))
                               r <- genExp s' `suchThat` (\r -> not(isReturn r))
                               return (Or l r)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l))
                               return (Not l)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l) && not(isLogic l))
                               return (Inc l)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l) && not(isLogic l))
                               return (Dec l)),
                        (s, do l <- genExp s' `suchThat` (\l -> not(isReturn l) && not(isLogic l))
                               return (Return l)),
                        (s, do b <- arbitrary
                               return (Bool b)),
                        (1, genVar)
                       ]
        where s' = div s 2
              genVar = do
                        str <- listOf (elements ['a' .. 'z']) `suchThat` (\s -> not (null s) && length s <= 5)
                        return $ Var str


instance Arbitrary Item where
    arbitrary = sized genItem
genItem s = frequency [(1, genDecl),
                       (1, genArg),
                       (1, genIncrement),
                       (1, genDecrement),
                       (1, genNestedIf),
                       (1, genOpenIf),
                       (1, genNestedWhile),
                       (1, genOpenWhile),
                       (1, genNestedLet),
                       (1, genOpenLet),
                       (1, genNestedFuncao),
                       (1, genOpenFuncao),
                       (1, genNestedReturn)
                      ]
        where genDecl = do
                            str <- listOf (elements ['a' .. 'z']) `suchThat` (\s -> not (null s) && length s <= 2)
                            n <- genExp 1 `suchThat` (\n -> not(isReturn n) && not(isLogic n))
                            return $ Decl str n
              genArg = do
                            exp <- genExp 1 `suchThat` (\exp -> (isVar exp) || (isConst exp))
                            return $ Arg exp
              genIncrement = do
                                exp <- genExp 1 `suchThat` (\exp -> not(isLogic exp))
                                return $ Increment exp
              genDecrement = do
                                exp <- genExp 1 `suchThat` (\exp -> not(isLogic exp))
                                return $ Decrement exp
              genNestedIf = do
                                exp <- genIf s
                                return $ NestedIf exp
              genOpenIf = do
                                exp <- genIf s
                                return $ OpenIf exp
              genNestedWhile = do
                                exp <- genWhile s
                                return $ NestedWhile exp
              genOpenWhile = do
                                exp <- genWhile s
                                return $ OpenWhile exp
              genNestedLet = do
                                exp <- genLet s
                                return $ NestedLet exp
              genOpenLet = do
                                exp <- genLet s
                                return $ OpenLet exp
              genNestedFuncao = do
                                exp <- genFuncao s
                                return $ NestedFuncao exp
              genOpenFuncao = do
                                exp <- genFuncao s
                                return $ OpenFuncao exp
              genNestedReturn = do
                                exp <- genExp 1 
                                return $ NestedReturn exp


genItems 0 = return []
genItems n = do h <- genItem 1 `suchThat` (\i -> not(isArg' i) && not(isDefFuncao' i))
                t <- genItems (n-1)
                return (h:t)

genItemsF 0 = return []
genItemsF 1 = do exp <- genExp 1 `suchThat` (\exp -> (isReturn exp) && not(isLogic exp))
                 return $ [NestedReturn exp]
genItemsF n = do h <- genItem 1 `suchThat` (\i -> not(isArg' i) && not(isDefFuncao' i))
                 t <- genItems (n-1)
                 return (h:t)

instance Arbitrary Let where
    arbitrary = sized genLet
genLet s = frequency [(1, genLet')]
        where genLet' = do
                            i <- genItems s
                            n <- listOf (elements ['a' .. 'z']) `suchThat` (\s -> not (null s) && length s <= 2)
                            return $ Let i (Var n)

instance Arbitrary Name where
    arbitrary = sized genName
genName s = frequency [(1, genName')]
        where genName' = do
                            n <- listOf (elements ['a' .. 'z']) `suchThat` (\s -> not (null s) && length s <= 5)
                            return $ Name n

genArgs 0 = return []
genArgs n = do h <- genItem 1 `suchThat` (\h -> (isArg' h) && not(isReturn' h))
               t <- genArgs (n-1) 
               return (h:t)

instance Arbitrary Funcao where
    arbitrary = sized genFuncao
genFuncao s = frequency [(s, genFuncao'),
                         (s, genDefFuncao')]
        where genFuncao' = do
                            n <- genName s
                            a <- genArgs s
                            return $ Funcao n a
              genDefFuncao' = do
                                n <- genName s
                                a <- genArgs s
                                i <- genItemsF s 
                                return $ DefFuncao n a i

instance Arbitrary If where
    arbitrary = sized genIf
genIf s = frequency [(1, genIf'),
                     (1, genElse')]
        where genIf' = do
                        e <- genExp s `suchThat` (\e -> not(isReturn e) && (isLogic e))
                        i <- genItems s
                        return $ If e i
              genElse' = do
                        i <- genExp s `suchThat` (\e -> not(isReturn e) && (isLogic e))
                        a <- genItems s
                        b <- genItems s
                        return $ Else i a b

instance Arbitrary While where
    arbitrary = sized genWhile
genWhile s = frequency [(1, genWhile')]
        where genWhile' = do
                            e <- genExp s `suchThat` (\e -> not(isReturn e) && (isLogic e))
                            i <- genItems s
                            return $ While e i

--Auxiliary functions
isReturn :: Exp -> Bool
isReturn x = case x of
  Return a -> True
  _ -> False

isReturn' :: Item -> Bool
isReturn' x = case x of
  Arg (Return a) -> True
  _ -> False

isOpen :: Item -> Bool
isOpen x = case x of
  OpenFuncao (DefFuncao a b c) -> True
  _ -> False

isArg' :: Item -> Bool
isArg' x = case x of
  Arg b -> True
  _ -> False

isDefFuncao' :: Item -> Bool
isDefFuncao' x = case x of
  NestedFuncao (DefFuncao a b c) -> True
  _ -> False

isVar :: Exp -> Bool
isVar x = case x of
  Var b -> True
  _ -> False

isConst :: Exp -> Bool
isConst x = case x of
  Const b -> True
  _ -> False

isZero :: Exp -> Bool
isZero x = case x of
  Const 0 -> True
  _ -> False

isLogic :: Exp -> Bool
isLogic x = case x of
       Less a b -> True
       Greater a b -> True 
       Equals a b -> True 
       GTE a b -> True 
       LTE a b -> True 
       And a b -> True 
       Or a b -> True 
       Not a -> True
       Bool a -> True
       _ -> False  

--Generator
genLang = do h <- genItem 3 `suchThat` (\h -> (isOpen h))
             return [(h,"")]

gen = sample (genLang)