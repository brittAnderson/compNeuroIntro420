#Question 1
#3 x 2 matrix
A <- matrix(c(1, 2,
              3, 4,
              5, 6),
            nrow = 3,
            byrow = TRUE)

#2 x 3 matrix
B <- matrix(c(7, 8, 9,
              10, 11, 12),
            nrow = 2,
            byrow = TRUE)

#First 2 x 2 matrix
C <- matrix(c(1, 2,
              3, 4),
            nrow = 2,
            byrow = TRUE)

#Second 2 x 2 matrix
D <- matrix(c(5, 6,
              7, 8),
            nrow = 2,
            byrow = TRUE)

#Show matrices
print(A)
print(B)
print(C)
print(D)

#Question 2
#Sum two 2 x 2 matrices

CD_sum <- C + D
print(CD_sum)

#Question 3
#Transpose 3 x 2 matrix

A_transpose <- t(A)
print(A_transpose)

#Question 4
#Add transpose to permissible matrix

A_transpose_plus_B <- A_transpose + B
print(A_transpose_plus_B)

#Question 5
#Multiply matrices to get a 3 x 3 matrix

AB_product <- A %*% B
print(AB_product)

#Question 6
#Dot product

column_vector <- matrix(c(1, 2, 3), ncol = 1)

row_vector <- matrix(c(9, 8, 7), nrow = 1)

dot_product <- row_vector %*% column_vector

print(dot_product)
