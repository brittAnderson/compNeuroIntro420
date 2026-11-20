import numpy as np

# 1
A = np.array([[1, 2],
              [3, 4],
              [5, 6]])            # 3 x 2

B = np.array([[1, 0, 2],
              [3, 1, 4]])         # 2 x 3

C = np.array([[1, 2],
              [3, 4]])            # 2 x 2

D = np.array([[5, 6],
              [7, 8]])            # 2 x 2

print("A (3x2):\n", A, "\n")
print("B (2x3):\n", B, "\n")
print("C (2x2):\n", C, "\n")
print("D (2x2):\n", D, "\n")

# 2
CD = C + D
print("C + D (2x2):\n", CD, "\n")

# 3
A_transposed = np.transpose(A)
print("Transpose of A (2x3):\n", A_transposed, "\n")

# 4
A_transposedB = A_transposed + B
print("A^T + B (2x3):\n", A_transposedB, "\n")

# 5
AxB = A @ B
print("A @ B (3x3):\n", AxB, "\n")

# 6
col_vec = np.array([[1],
                    [2],
                    [3]])       # 3 x 1
row_vec = np.array([[4, 5, 6]])  # 1 x 3

dot_product = row_vec @ col_vec
print("Column vector (3x1):\n", col_vec, "\n")
print("Row vector (1x3):\n", row_vec, "\n")
print("Dot product (row . column):", dot_product[0, 0])