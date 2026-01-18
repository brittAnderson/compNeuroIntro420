module MyMats where

a = [[1,2,3],[4,5,6]]
b = [[-1,-2,-3],[4,5,6]]
c = [[1,2],[3,4],[5,6]]           -- 3x2 matrix
d = [[7,8,9],[10,11,12]]          -- 2x3 matrix
e = [[1,0],[0,1]]                 -- 2x2 identity matrix
f = [[2,3],[4,5]]                 -- 2x2 matrix
g = [[1]]                         -- 1x1 matrix
h = [[1,2,3]]                     -- 1x3 matrix (row vector)
i = [[1],[2],[3]]                 -- 3x1 matrix (column vector)
j = [[0,0],[0,0]]                 -- 2x2 zero matrix


type SimpMat a = [[a]]

add2SimpMats :: Num a => SimpMat a -> SimpMat a -> SimpMat a
add2SimpMats = zipWith (zipWith (+))

rotateLList :: [[a]] -> [[a]]
rotateLList [] = []
rotateLList [[],_] = []
rotateLList l = fmap head l : rotateLList (map tail l)



multSimpMats :: Num a => SimpMat a -> SimpMat a -> SimpMat a
multSimpMats m1 m2 =
  [ [dotProd r c | c <- m2Transposed ] | r <- m1 ]
  where dotProd v1 v2 = sum (zipWith (*) v1 v2)
        m2Transposed = rotateLList m2
