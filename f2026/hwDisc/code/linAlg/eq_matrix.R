#1. 3x2, 2x3, two 2x2
a <- matrix(1:6, nrow = 3, ncol = 2)
b <- matrix(c(2,4,6,8,10,12), nrow = 2, ncol = 3)
c <- matrix(c(3,6,9,12), nrow = 2, ncol = 2)
d <- matrix(c(4,8,12,16), nrow = 2, ncol = 2)

#2. I'm able to add c and d as they have the same # of rows and columns
c + d

#3. transposing my 3x2
t(a)

#4.adding the transposed martix to the 2x3
t(a)+b

#5.
a %*% b

#6. 3x1, and 1x3
column3 <- matrix(1:3, nrow = 3)
row3 <- matrix(3:5, ncol = 3)

#the dot product = (1x3)*(3x1)
row3 %*% column3
