A = [[1, 2],
    [3, 0],
    [4, 0]]

B = [[1, 1, 2],
    [1, 2, 1]]

C = [[1, 2],
    [0, 1]]

D = [[5, 6],
    [7, 8]]

def print_matrix(label, matrix):
    print(label)
    for row in matrix:
        print(row)
    print()

print_matrix("A:", A)
print_matrix("B:", B)
print_matrix("C:", C)
print_matrix("D:", D)

# C + D = [[1+5, 2+6], [0+7, 1+8]]
#       = [[6, 8], [7, 9]].
sum_2x2 = []
for i in range(len(C)):
    row = []
    for j in range(len(C[0])):
        row.append(C[i][j] + D[i][j])
    sum_2x2.append(row)

print_matrix("C + D:", sum_2x2)

# swap rows/columns
transpose_a = []
for j in range(len(A[0])):
    row = []
    for i in range(len(A)):
        row.append(A[i][j])
    transpose_a.append(row)

print_matrix("A^T:", transpose_a)

# A^T + B = [[1+1, 3+1, 4+2], [2+1, 0+2, 0+1]]
#         = [[2, 4, 6], [3, 2, 1]].
sum_transpose_b = []
for i in range(len(transpose_a)):
    row = []
    for j in range(len(transpose_a[0])):
        row.append(transpose_a[i][j] + B[i][j])
    sum_transpose_b.append(row)

print_matrix("A^T + B:", sum_transpose_b)

# Multiply each A row by B col; add products
# ex cell[0][0] = A[0][0]*B[0][0] + A[0][1]*B[1][0] = 1*1 + 2*1 = 3.
product_ab = []
for i in range(len(A)):
    row = []
    for j in range(len(B[0])):
        total = 0
        for k in range(len(B)):
            total += A[i][k] * B[k][j]
        row.append(total)
    product_ab.append(row)

print_matrix("A*B:", product_ab)

column_vector = [[1],
                 [2],
                 [3]]
row_vector = [[4, 5, 6]]

# [4, 5, 6] . [1, 2, 3] = 4*1 + 5*2 + 6*3
#                       = 4 + 10 + 18 = 32.
dot_product = 0
for i in range(len(column_vector)):
    dot_product += row_vector[0][i] * column_vector[i][0]

print_matrix("Column vector:", column_vector)
print_matrix("Row vector:", row_vector)
print("Dot product:", dot_product)
