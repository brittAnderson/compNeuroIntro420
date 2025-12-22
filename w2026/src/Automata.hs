module Automata where

import Text.Printf (printf)
import Numeric (showIntAtBase)
import Data.Char (intToDigit,digitToInt)
import Data.List (foldl')

int2bin :: Int -> String
int2bin =  printf "%08b" 

bin2int :: String -> Int
bin2int =
  foldl' (\ accum dig -> accum * 2 + digitToInt dig ) 0 

myelems :: [ String ]
myelems = ["0","1"]

myf :: Char -> Char
myf x = if x == '0' then 'w' else 'b'

myClrLst :: [String]
myClrLst = map (map myf) [x ++ y ++ z | x <- myelems, y <- myelems, z <- myelems]

newtype Rule = Rule [(String,Char)] deriving Show

makeRule :: Int -> Rule
makeRule myrulenum =
  let mcl = myClrLst
      blst = int2bin myrulenum
  in Rule $ zip mcl $ map myf blst



