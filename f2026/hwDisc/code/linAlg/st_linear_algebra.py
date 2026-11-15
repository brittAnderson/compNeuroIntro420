import numpy as np

matrix_a = np.array([
    [1, 2],
    [3, 4],
    [5, 6]
])

matrix_b = np.array([
    [7, 8, 9],
    [10, 11, 12]
])

matrix_c = np.array([
    [13, 14],
    [15, 16]
])

matrix_d = np.array([
    [17, 18],
    [19, 20]
])

result_add = matrix_c + matrix_d
print(result_add)

transposed = np.transpose(matrix_a)
print(transposed)

result_add2 = matrix_b + transposed
print(result_add2)

result_mult = matrix_a @ matrix_b
print(result_mult)

column_vector = np.array([
    [1],
    [2],
    [3]
])

row_vector = np.array([
    [4, 5, 6]
])

result_dot = column_vector @ row_vector
print(result_dot)