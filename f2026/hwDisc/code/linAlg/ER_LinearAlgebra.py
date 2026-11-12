import numpy as np

# Matrix creation
matrix3x2 = np.array([[1, 2, 3],
                  [3, 4, 5]])

matrix2x3 = np.array([[1, 2],
                  [3, 4],
                  [5, 6]])

matrix2x2_1 = np.array([[1, 2],
                  [3, 4]])

matrix2x2_2 = np.array([[5, 6],
                  [7, 8]])

# Matrix addition function
def add_matrices(matrix1, matrix2):
    return np.add(matrix1, matrix2)

# Adds the 2 2x2 matricies
added_arrays = add_matrices(matrix2x2_1, matrix2x2_2)
print("Added Matrices:\n", added_arrays)

# Transpose Arrays, note that any 2D array can be transposed, not just square matrices
transpose_3x2 = np.transpose(matrix3x2)

# Adding transposed matrix to other matrices, possible for any matricies of opposite (i.e 2x3 and 3x2) dimensions
transpose_addition = add_matrices(transpose_3x2, matrix2x3)
print("Transpose Addition Result:\n", transpose_addition)

# Matrix multiplication function
def multiply_matrices(matrix1, matrix2):
    return matrix1 @ matrix2

# Multiplying 2x3 and 3x2 matrices with the 3's point inward, produces a 3x3 matrix
multipliedMatricies = multiply_matrices(matrix2x3, matrix3x2)
print("3x3 Matrix from Multiplication:\n", multipliedMatricies)

# Vector Creation
colVector = np.array([1, 2, 3]).reshape(3,1)
rowVector = np.array([1, 2, 3]).reshape(1,3)

# Dot product function
def dot_product(matrix1, matrix2):
    return np.dot(matrix1, matrix2)

# Calculating the dot product of the column and row vectors
vectorDotProduct = dot_product(colVector, rowVector)
print("Dot Product:\n", vectorDotProduct)