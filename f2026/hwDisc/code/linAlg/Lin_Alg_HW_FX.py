import numpy as np

#I used the np.array class over the np.matrix class because the latter 

#A is a 3x2 matrix, B is a 2x3 matrix
A = np.array([[1,2,3], [4,5,6]])
B = np.array([[0,9], [8,7], [6,5]])

#Two 2x2 matrices
C = np.array([[3,2], [7,8]])
D = np.array([[4,5], [6,7]])

#adds the two vectors of the same size
addition = C + D
print("1.\n", addition)

#transposes A
A_Transpose = np.matrix_transpose(A)
print("2.\n", A_Transpose)

#Adds A_Tranpose to B
E = A_Transpose + B
print("3.\n", E)

#Multiplies A and B to get a 3x3 matrix
F = np.linalg.matmul(B, A)
print("4.\n", F)

#Creates a 3x1 and 1x3 vector, and takes their dot product
#I chose the identity matrix, because the dot 
I_3 = np.identity(3)

col = I_3[:,0]
row = I_3[1]
print("5.\n",np.vecdot(col, row))