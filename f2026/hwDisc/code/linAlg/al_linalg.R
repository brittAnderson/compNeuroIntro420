# create 3x2, 2x3, and (two) 2x2 matrices; should be possible to add matrices together.
threeByTwo <- matrix(1:6, nrow = 3, ncol = 2)
print(threeByTwo)

twoByThree <- matrix(1:6, nrow = 2, ncol = 3)
print(twoByThree)

twoByTwo1 <- matrix (1:4, nrow = 2, ncol = 2)
print(twoByTwo1)

twoByTwo2 <- matrix(1:4, nrow = 2, ncol = 2)
print(twoByTwo2)

# shape, column vector, row vector had assistance from Claude.
# code that adds matrices together
combine <- twoByTwo1 + twoByTwo2
print(combine)

# transpose
transposed <- t(threeByTwo)
print(transposed)

# shape
# shape of 3 x 2, now transposed to 2 x 3

addTransposed <- transposed + twoByThree
print(addTransposed)

# multiply
multiplied <- threeByTwo %*% twoByThree
print(multiplied)

# column vector of dimension three
columnVector <- matrix(1:3, nrow = 3, ncol = 1)
print(columnVector)

# row vector of same column
rowVector <- matrix(1:3, nrow = 1, ncol = 3)
print(rowVector)

# "dot" product
dot <- rowVector %*% columnVector
print(dot)