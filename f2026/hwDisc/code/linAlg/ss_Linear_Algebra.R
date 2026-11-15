A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 3, ncol = 2)
B <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
C <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)
D <- matrix(c(5, 6, 7, 8), nrow = 2, ncol = 2)

print(C + D)

A_t <- t(A)
print(A_t)

print(A_t + B)

print(A %*% B)

col_vec <- matrix(c(1, 2, 3), nrow = 3, ncol = 1)
row_vec <- matrix(c(4, 5, 6), nrow = 1, ncol = 3)
print(row_vec %*% col_vec)
