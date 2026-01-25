{-# OPTIONS_GHC -Wno-unused-matches #-}
module MyMats where

mata :: SimpMat Integer
mata = [[1,2,3],[4,5,6]]
matb :: SimpMat Integer
matb = [[-1,-2,-3],[4,5,6]]
matc :: SimpMat Integer
matc = [[1,2],[3,4],[5,6]]           -- 3x2 matrix
matd :: SimpMat Integer
matd = [[7,8,9],[10,11,12]]          -- 2x3 matrix
mate :: SimpMat Integer
mate = [[1,0],[0,1]]                 -- 2x2 identity matrix
matf :: SimpMat Integer
matf = [[2,3],[4,5]]                 -- 2x2 matrix
matg :: SimpMat Integer
matg = [[1]]                         -- 1x1 matrix
math :: SimpMat Integer
math = [[1,2,3]]                     -- 1x3 matrix (row vector)
mati :: SimpMat Integer
mati = [[1],[2],[3]]                 -- 3x1 matrix (column vector)
matj :: SimpMat Integer
matj = [[0,0],[0,0]]                 -- 2x2 zero matrix


type SimpMat a = [[a]]

prettyMat :: Show a => SimpMat a -> String
prettyMat m = unlines $ map show m

add2SimpMats :: Num a => SimpMat a -> SimpMat a -> SimpMat a
add2SimpMats = zipWith (zipWith (+))

rotateLList :: SimpMat a -> SimpMat a
rotateLList [] = []
rotateLList l | any null l = []
rotateLList l = fmap head l : rotateLList (map tail l)

vectMult :: Num a => SimpMat a -> SimpMat a -> SimpMat a
vectMult v1 v2 =
  let v1' = if length v1 == 1 then v1 else rotateLList v1
      v2' = if length v2 == 1 then rotateLList v2 else v2
  in multSimpMats v1' v2'

multSimpMats :: Num a => SimpMat a -> SimpMat a -> SimpMat a
multSimpMats m1 m2
  | null m1 || null m2 =
    error "Cannot multiply empty matrices"
  | cols1 /= rows2 =
    error $ "Dimension mismatch: " ++
    show cols1 ++ " ≠ " ++ show rows2
  | otherwise = [ [dotProd row col | col <- m2Transposed ] | row <- m1 ]
  where
    cols1 = length (head m1)
    rows2 = length m2
    dotProd v1 v2 = sum (zipWith (*) v1 v2)
    m2Transposed = rotateLList m2

binary2Bipolar :: (Ord a,Num a) => SimpMat a -> SimpMat a
binary2Bipolar inmat =
  [[ if x <= 0 then -1 else 1  | x <- row ]  | row <- inmat ]

zeroDiagonal :: Num a => [[a]] -> [[a]]
zeroDiagonal  = zipWith zeroDiagRow [0..] 
  where
    zeroDiagRow i  = zipWith (\j x -> if i == j then 0 else x) [0..]

scalarMultiply :: Num a => a -> SimpMat a -> SimpMat a
scalarMultiply eta inmat =
  [[ eta * j | j <- row ] | row <- inmat]

