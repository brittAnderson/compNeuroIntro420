import numpy as np

## 1. Create 3 x 2, 2 x 3, and (two) 2 x 2 matrices. 

hi = 10

A_3x2 = np.random.randint(hi, size = (3,2))
A_2x3 = np.random.randint(hi, size = (2,3))

A_2x2 = np.random.randint(hi, size = (2,2))
B_2x2 = np.random.randint(hi, size = (2,2))


## 6. Create a column vector of dimension three, and a row vector of the 
    # same dimension. 

row_3 = np.random.randint(hi, size = (3,))
col_3 = np.random.randint(hi, size = (3,1))


## 2. It should be possible to add two of your for matrices.
    # Write the code that adds two eligible matrices together. 

def matrix_add(matrix1, matrix2):
    dim1 = matrix1.shape
    dim2 = matrix2.shape

    if dim1 != dim2:
        print("Matrices must have the same dimensions :(\n")
    else:
        summation = np.zeros_like(matrix1)
        rows, cols = matrix1.shape

        for i in range(rows):
            for j in range(cols):
                summation[i][j] = matrix1[i][j] + matrix2[i][j]

        print(f"The sum of the two {rows} x {cols} matrices is\n {matrix1}\n +\n {matrix2}\n =\n {summation}\n")
        return summation


def transpose(matrix):
    rows, cols = matrix.shape 
    transposed = np.zeros((cols, rows), dtype = matrix.dtype)

    for i in range(rows):
        for j in range(cols):
            transposed[j][i] = matrix[i][j]

    print(f"The transpose of the {rows} x {cols} matrix is\n {transposed}\n")
    return transposed


def matrix_mult(matrix1, matrix2):
    inner1, inner2 = matrix1.shape[1], matrix2.shape[0]
    if inner1 != inner2:
        print("Matrices must have corresponding dimensions :((\n")
    else:
        outer1, outer2 = matrix1.shape[0], matrix2.shape[1]
        produced = np.zeros((outer1, outer2), dtype = matrix1.dtype)

        for i in range(outer1): # rows of matrix 1
            for j in range(outer2): # columns of matrix 2
                for k in range(inner1): # shared dimension
                    produced[i][j] += matrix1[i][k] * matrix2 [k][j]

        print(f"The matrix multiplication product of the two matrices is\n {matrix1}\n ⋅\n {matrix2}\n =\n {produced}\n")
        return produced


def dot_prod(row_vec, col_vec):
    col_flat = col_vec[:, 0].copy()

    dim1 = row_vec.shape
    dim2 = col_flat.shape

    if dim1 != dim2:
        print("Matrices must have the same number of dimensions :(((")
    else:
        dotted = sum(row_vec[i] * col_flat[i] for i in range(len(row_vec)))
        print(f"The dot product of the two matrices is\n {row_vec}\n ⋅\n {col_vec}\n =\n {dotted}\n")
        return dotted


## Function Calls !

matrix_add(A_2x2, A_2x3) # Case wherein matrix addition is not possible
matrix_add(A_2x2, B_2x2)


## 3. Create the transpose of the 3 x 2 matrix. 
A_3x2_transposed = transpose(A_3x2)


## 4. Add the transpose to the other of the three matrices
    # that is permissible. 
matrix_add(A_2x3, A_3x2_transposed)


## 5. Find the two matrices that when multiplied together will
    # result in a 3 x 3 matrix. 

matrix_mult(A_3x2, A_2x3)

## 6. Create a column vector of dimension three, and a row vector of the 
    # same dimension. Take their "dot" product.

dot_prod(row_3, col_3)