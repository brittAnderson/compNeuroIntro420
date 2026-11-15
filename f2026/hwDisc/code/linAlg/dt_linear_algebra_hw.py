import numpy as np
import numpy.linalg as LA

#Create 3x2, 2x3, and two 2x2 matrices

#3x2
A = np.array([[1, 2],
              [3, 4],
              [5, 6]])      

#2x3
B = np.array([[7, 8, 9],
              [10, 11, 12]]) 

#2x2
C = np.array([[1, 2],
              [3, 4]])     

#2x2
D = np.array([[5, 6],
              [7, 8]])     

print("Matrix A (3x2):")
print(A)

print("Matrix B (2x3):")
print(B)

print("Matrix C (2x2):")
print(C)

print("Matrix D (2x2):")
print(D)

#The two 2x2 matrices are the matrices you can add 
addition_result = C + D

print("C + D =")
print(addition_result)

#Making a transpose of the 3x2 matrix
A_transpose = A.T

print("Transpose of A:")
print(A_transpose)

#Adding the transpose to the other matrix (B)
transpose_addition = A_transpose + B

print("A^T + B =")
print(transpose_addition)

#Multiplying matrices to make a 3x3 matrix
#A is 3x2 and B is 2x3, so AB is 3x3
product_result = np.matmul(A, B)

print("A x B =")
print(product_result)

#Column and row vectors of dimension 3
column_vector = np.array([[1],
                          [2],
                          [3]])

row_vector = np.array([[4, 5, 6]])

dot_product = np.dot(row_vector, column_vector)

print("Row Vector:")
print(row_vector)

print("Column Vector:")
print(column_vector)

print("Dot Product:")
print(dot_product)
