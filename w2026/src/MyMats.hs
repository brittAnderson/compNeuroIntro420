module MyMats where

a :: [[Integer]]
a = [[1,2,3],[4,5,6]]
b :: [[Integer]]
b = [[-1,-2,-3],[4,5,6]]
c :: [[Integer]]
c = [[1,2],[3,4],[5,6]]           -- 3x2 matrix
d :: [[Integer]]
d = [[7,8,9],[10,11,12]]          -- 2x3 matrix
e :: [[Integer]]
e = [[1,0],[0,1]]                 -- 2x2 identity matrix
f :: [[Integer]]
f = [[2,3],[4,5]]                 -- 2x2 matrix
g :: [[Integer]]
g = [[1]]                         -- 1x1 matrix
h :: [[Integer]]
h = [[1,2,3]]                     -- 1x3 matrix (row vector)
i :: [[Integer]]
i = [[1],[2],[3]]                 -- 3x1 matrix (column vector)
j :: [[Integer]]
j = [[0,0],[0,0]]                 -- 2x2 zero matrix


type SimpMat a = [[a]]

add2SimpMats :: Num a => SimpMat a -> SimpMat a -> SimpMat a
add2SimpMats = zipWith (zipWith (+))

rotateLList :: SimpMat a -> SimpMat a
rotateLList [] = []
rotateLList l | any null l = []
rotateLList l = fmap head l : rotateLList (map tail l)


multSimpMats :: Num a => SimpMat a -> SimpMat a -> SimpMat a
multSimpMats m1 m2 
  | null m1 || null m2 = error "Cannot multiply empty matrices"
  | cols1 /= rows2 = error $ "Dimension mismatch: " ++ show cols1 ++ " ≠ " ++ show rows2
  | otherwise = [ [dotProd row col | col <- m2Transposed ] | row <- m1 ]
  where
    cols1 = length (head m1)
    rows2 = length m2
    dotProd v1 v2 = sum (zipWith (*) v1 v2)
    m2Transposed = rotateLList m2

