
# Asked Google (Gemini) on how a 3x2 matrix in python looks like and then I worked from there for the other matrices

import numpy as np

#Step 1, creating matrices
#3x2 matrix

matrixa = np.array([[1, 2], [3, 4], [5, 6]])

#2x3 matrix

matrixb = np.array([[1, 2, 3], [4, 5, 6]])

#2x2 matrices

matrixc = np.array([[1, 2], [3, 4]])

matrixd = np.array([[1, 2], [3, 4]])

#Step 2: Need to add matrices together, they need to be same dimension

step2_result = matrixc + matrixd

print("Step 2 addition result:", 
step2_result)

#Step 3, Transposing 3x2 matrix: learned from Gemini that to transpose, you need to swap the dimensions, so we need to make it go from 3 rows and 2 columns, to 3 columns and 2 rows

#Asked Gemini how to use transpose in code because I could not guess it, I thought I had to rewrite the dimensions myself

#Learned that T is like the superscript T in linear algebra

transposed_matrixa = matrixa.T

#Step 4, Adding transposed matrixa to matrixb since thats the only one that matches its dimensions (2x3)

step4_result = transposed_matrixa + matrixb

print("Step 4 addition result:", 
step4_result)

#Step 5, adding matrixa and matrixb to get a 3x3 matrix
# it becomes a 3x3 because there's 3 rows in the first matrix and 3 columns in the second, and then since you have to multiply each row by each column it naturally creates a 3x3 grid
# Gemini corrected my attempt at using * instead of @ when trying to multiply 

matrixe = matrixa @ matrixb

print("Step 5 multiplication result:", 
matrixe)

#Step 6, Creating column and row vector and taking dot product
#Had to use Gemini for this step I was very confused on the instructions here

row_vector = matrixe[0, :]

col_vector = matrixe[:, 0]

#Assuming you can also use @ but looked up dot to fit requirements

dot_result = np.dot(row_vector, col_vector)

print("Dot product:", dot_result)




