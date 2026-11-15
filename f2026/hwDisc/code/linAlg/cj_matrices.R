## 1. Create 3 x 2, 2 x 3, and (two) 2 x 2 matrices.

matrix.3x2 <- matrix(c(1,2,3,2,4,6), nrow=3, ncol=2)
matrix.3x2

matrix.2x3 <- matrix(c(1,4,2,5,3,6), nrow=2, ncol=3)
matrix.2x3

matrix.2x2.a <- matrix(c(6,8,10,12), nrow=2, ncol=2)
matrix.2x2.a

matrix.2x2.b <- matrix(c(2,4,6,8), nrow=2, ncol=2)
matrix.2x2.b

## 2. It should be possible to add two of your for matrices. Write the
##    code that adds the two eligible matrices together.

matrix.2x2.c <- matrix.2x2.a + matrix.2x2.b
matrix.2x2.c

## 3. Create the transpose of your 3 x 2 matrix.

matrix.2x3.b <- t(matrix.3x2)
matrix.2x3.b

## 4. Add the transpose to the other of your three matrices that is 
##    permissible.

matrix.3x2.b <- t(matrix.2x3)
matrix.3x2.b

## 5. Find the two matrices that when multiplied together will leave you
##    with a 3 x 3 matrix. Write the code that multiples them.

matrix.3x3 <- matrix.3x2.b %*% matrix.2x3.b  #column x row: illegal matrix
matrix.3x3

## 6. Create a column vector of dimension three, and a row vector of the
##    same dimension. Take their "dot" product.

column.vector <- matrix(c(1,2,3), nrow=3, ncol=1)
row.vector <- matrix(c(4,5,6), nrow=1, ncol=3)

dot.product <- row.vector %*% column.vector #row x column: dot product
dot.product
