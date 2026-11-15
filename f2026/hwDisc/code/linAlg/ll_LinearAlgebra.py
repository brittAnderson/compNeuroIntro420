"""
QUICK GUIDE FOR USING THIS PYTHON CODE!

This guide exists to simplify the user experience,
relying on what questions are answered rather than my own variables.

Each question will be answered using the following answer key...

Question 1:
    "Q1A" -> 3x2 matrix
    "Q1B" -> 2x3 matrix
    "Q1C" -> 2x2 matrix
    "Q1D" -> 2x2 matrix

Question 2:
    "Q2" -> Addition of two 2x2 matrices (C and D)

Question 3:
    "Q3" -> transpoition of a 3x2 matrix

Question 4:
    "Q4B" -> transposition of a 2x3 matrix
    "Q4C" -> transposition of a 2x2 matrix
    "Q4D" -> transposition of a 2x2 matrix (again)

Question 5:
    "Q5" -> multiplying 3x2 and 2x3 matrices, creating a 3x3 matrix

Question 6:
    "Q6A" -> column vector of dimension three
    "Q6B" ->row vector of dimension three
    "Q6C" -> "dot" product of column vector and row vector
"""



#imports
import numpy as np


#1, creating matrices
A = np.array([[1,2,3],
              [4,5,6]])     # 3x2

B = np.array([[1,2],
               [3,4],
               [5,6]])      # 2x3

C = np.array([[1,2],
               [3,4]])      # 2x2

D = np.array([[5,6],
               [7,8]])      # 2x2


#2, demonstrating addition
E = C + D


#3, demonstrating transposition with 3x2 matrix
AT = np.transpose(A) 


#4, demonstrating transposition with the other matrices
BT = np.transpose(B)        #2x3
CT = np.transpose(C)        #2x2
DT = np.transpose(D)        #2x2


#5, demonstrating multiplication, creating a 3x3 matrix
F = B @ A


#6, demonstrating column vector of dimension three
G = np.array([[1],
             [2],
             [3]])


#6, demonstrating row vector of dimension three
H = np.array([1,2,3])


#6, "dot" product of column vector and row vector of dimension three
I = np.dot(H,G)





# quick guide components
Q1A = A
Q1B = B
Q1C = C
Q1D = D
Q2 = E
Q3 = AT
Q4B = BT
Q4C = CT
Q4D = DT
Q5 = F
Q6A = G
Q6B = H
Q6C = I
