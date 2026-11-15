import numpy as np

A = np.array([[1, 2], [3, 4], [5, 6]])

B = np.array([[9, 8, 7], [6, 5, 4]])

C = np.array([[1, 3], [5, 7]])

D = np.array([[2, 4], [6, 8]])

print("Question 1")
print ("A")
print(A)
print("B")
print(B)
print("C")
print(C)
print("D")
print(D)

#to add them together you need the same number of rows and columns, so only the 2x2 matrices (C and D) can be added together
Q_Two = C + D

print ("Question 2")
print(Q_Two)

print ("Question 3")
print(A.T)

Q_Four = A.T + B

print ("Question 4")
print(Q_Four)

Q_Five = A @ B

print ("Question 5")
print(Q_Five)

E = np.array ([[2], [4], [6]])
F = np.array([3, 5, 7])

Q_Six = np.dot(F, E)

print ("Question 6")
print(Q_Six)