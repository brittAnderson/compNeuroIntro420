# -*- coding: utf-8 -*-
"""
Created on Thu Nov 19 19:27:57 2026

@author: rukhs
"""
import numpy as np

# 1. Create a 3x2, a 2x3, and two 2x2 matrices
# Note: Dimensions are always (rows, columns)
mat_3x2 = np.array([[1, 2], 
                    [3, 4], 
                    [5, 6]])

mat_2x3 = np.array([[1, 2, 3], 
                    [4, 5, 6]])

mat_2x2_a = np.array([[1, 2], 
                      [3, 4]])

mat_2x2_b = np.array([[5, 6], 
                      [7, 8]])

# 2. Add the two eligible matrices together
# You can only add matrices that have the exact same dimensions (the two 2x2 matrices)
sum_2x2 = mat_2x2_a + mat_2x2_b
print("Step 2 - Sum of 2x2 matrices:\n", sum_2x2, "\n")

# 3. Create the transpose of your 3x2 matrix
# Transposing flips rows into columns, turning a 3x2 matrix into a 2x3 matrix
mat_3x2_transposed = mat_3x2.T
print("Step 3 - Transposed 3x2 matrix (now 2x3):\n", mat_3x2_transposed, "\n")

# 4. Add the transpose to the other matrix that is permissible
# Since the transposed matrix is now 2x3, we can add it to our original 2x3 matrix
sum_2x3 = mat_3x2_transposed + mat_2x3
print("Step 4 - Sum of transposed matrix and 2x3 matrix:\n", sum_2x3, "\n")

# 5. Find the two matrices that multiply to leave a 3x3 matrix, and multiply them
# Rule: (3x2) multiplied by (2x3) gives a (3x3) matrix. Use the '@' symbol for matrix multiplication.
prod_3x3 = mat_3x2 @ mat_2x3
print("Step 5 - Product resulting in a 3x3 matrix:\n", prod_3x3, "\n")

# 6. Create a column vector (dim 3) and a row vector (dim 3), and take their dot product
col_vec = np.array([[1], 
                    [2], 
                    [3]])  # 3 rows, 1 column

row_vec = np.array([[4, 5, 6]])  # 1 row, 3 columns

# A row vector (1x3) dotted with a column vector (3x1) gives a single scalar value (1x1)
dot_prod = np.dot(row_vec, col_vec)
print("Step 6 - Dot product:\n", dot_prod)
