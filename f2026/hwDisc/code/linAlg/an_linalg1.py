#matrixes
#(3x2)
A = [[1, 2],[3, 4],[5, 6]]

#(2x3)
B = [[7, 8, 9],[1, 12, 2]]

#(2x2)
C = [[1, 2],[3, 4]]

#(2x2)
D = [[5, 6],[7, 8]]

#sum of C and D 
CDsum = [ [C[0][0] + D[0][0], C[0][1] + D[0][1]],[C[1][0] + D[1][0], C[1][1] + D[1][1]]]
print("The sum of C and D", CDsum)

# transpose of your 3 x 2 matrix (turn the culumns and rows)
Atranspose = [[A[0][0], A[1][0], A[2][0]],[A[0][1], A[1][1], A[2][1]]]

AtransposeplusB = [[Atranspose[0][0] + B[0][0],Atranspose[0][1] + B[0][1],Atranspose[0][2] + B[0][2]],[Atranspose[1][0] + B[1][0],Atranspose[1][1] + B[1][1],Atranspose[1][2] + B[1][2]]]
print("Transpose of A =", Atranspose)
print("Transpose(A) + B =",AtransposeplusB)

#3x3 matrix
tbtmatrix= [
    [(A[0][0] * B[0][0]) + (A[0][1] * B[1][0]),(A[0][0] * B[0][1]) + (A[0][1] * B[1][1]),(A[0][0] * B[0][2]) + (A[0][1] * B[1][2])],
    [(A[1][0] * B[0][0]) + (A[1][1] * B[1][0]),(A[1][0] * B[0][1]) + (A[1][1] * B[1][1]),(A[1][0] * B[0][2]) + (A[1][1] * B[1][2])],
    [(A[2][0] * B[0][0]) + (A[2][1] * B[1][0]),(A[2][0] * B[0][1]) + (A[2][1] * B[1][1]),(A[2][0] * B[0][2]) + (A[2][1] * B[1][2])]]

print("To get a 3x3 matrix we can multiply A and B ")
print ("A x B =",tbtmatrix)

#column vector of dimension three, and a row vector of the same dimension.
rowVector = [1, 2, 3]
print ("The row vector is:",rowVector)
columnVector = [[1],[2],[3]]
print("The column vector is:", columnVector)

dot= ((rowVector[0] * columnVector[0][0]) +(rowVector[1] * columnVector[1][0]) +(rowVector[2] * columnVector[2][0]))

print("Dot product =", dot)