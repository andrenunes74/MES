{-# LANGUAGE DeriveDataTypeable #-}
module Testing where
import MutantGen
import Core
import Reverse
import Generator
import Opt
import Tools
import Data.Data
import Data.Generics.Zipper
import Library.Ztrategic
import Library.StrategicData (StrategicData)
import Test.QuickCheck hiding (Args)

--testar se diferentes estratégias de otimização de expressões aritméticas são equivalentes (passed)
prop_1 :: [(Item,String)] -> Bool
prop_1 x = optWithZippersTD(x) == optWithZippersBU(x)

--testar se uma expressão fica igual após otimizaçoes e eliminação de smells (trivialmente falsa)
prop_2 :: [(Item,String)] -> Bool
prop_2 x = x == optWithZippersTD(pParse(reverseP x))

--testar se uma expressão fica igual após eliminação de smells (trivialmente falsa)
prop_3 :: [(Item,String)] -> Bool
prop_3 x = x == pParse(reverseP x)

--testar se eliminação de smells e a optimização com a estratégia (full bottom up) de expressões aritméticas é equivalente (passed)
prop_4 :: [(Item,String)] -> Bool
prop_4 x = pParse(reverseP x) == optWithZippersBU(pParse(reverseP x)) 

--testar se eliminação de smells e a optimização de com a estratégia (full top down) expressões aritméticas é equivalente (passed)
prop_5 :: [(Item,String)] -> Bool
prop_5 x = pParse(reverseP x) == optWithZippersTD(pParse(reverseP x))

