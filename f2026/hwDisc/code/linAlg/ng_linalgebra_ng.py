
import numpy as np

# 1. Create four matrices
A = np.array([[1, 2],
              [3, 4],
              [5, 6]])  # 3 x 2

B = np.array([[1, 2, 3],
              [4, 5, 6]])  # 2 x 3

C = np.array([[1, 2],
              [3, 4]])  # 2 x 2

D = np.array([[5, 6],
              [7, 8]])  # 2 x 2

print("Matrix A:\n", A)
print("Matrix B:\n", B)
print("Matrix C:\n", C)
print("Matrix D:\n", D)

# 2. Add the two 2 x 2 matrices.
print("C + D:\n", C + D)

# 3. Find the transpose of A
A_transpose = A.T
print("Transpose of A:\n", A_transpose)

# 4. Add the transpose of A to B
print("Transpose of A + B:\n", A_transpose + B)

# 5. Multiply A and B to get a 3 x 3 matrix
print("A x B:\n", A @ B)

# 6. Create a column vector and a row vector
column_vector = np.array([[1],
                          [2],
                          [3]])

row_vector = np.array([[4, 5, 6]])

# Calculate the dot product
answer = np.dot(column_vector.flatten(),
                row_vector.flatten())

print("Column vector:\n", column_vector)
print("Row vector:\n", row_vector)
print("Dot product:", answer)

