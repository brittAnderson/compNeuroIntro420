import numpy as np
class matrix:

    A = np.array ([[1, 4, 7],
                   [2, 5, 8]])

    B = np.array ([[4, 4],
                   [7,7],
                   [9, 9]])

    C = np.array ([[2, 3],
                  [4, 5]])

    D = np.array([[5, 6],
                 [7, 8]])

def add(a, b):
    return a + b

def transpose(a):
    return a.T #the .T is imported from numpy to transpose matrices

def multiply(a, b):
    return a @ b #The @ operator is used for multiplying rather than simple *

print(add(matrix.C, matrix.D)) #2
print()
print(transpose(matrix.B)) #3
print()
print(add(transpose(matrix.B), matrix.A)) #4
print()
print(multiply(matrix.A, matrix.B)) #5
print()
V1 = np.array([[1], #column vector
               [3],
               [5]])
V2 = np.array([[2, 4, 6]]) #row vector

print(multiply(V2, V1)) #6