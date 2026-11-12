#Step 1. 
#3x2
A<-matrix(c(3,6,9,4,2,3), nrow = 3, ncol = 2)
A
#2x3
B<-matrix(c(4,6,16,12,15,20), nrow = 2, ncol = 3)
B
#(two) 2x2
C<-matrix(c(4,5,9,10), nrow = 2, ncol = 2)
C
D<-matrix(c(2,7,9,3), nrow = 2, ncol = 2)
D

#Step 2: Add two Eligible matrices
C+D
#Step 3: Transpose 3x2 (A)
t(A)
#Step 4: Add Transpose to Eligible three 
t(A)+B
#Step 5: Multiply and get 3x3
A%*%B
#Step 6: Dot Product 
x<-matrix(c(8,4,1), nrow = 3, ncol = 1)
x
y<-matrix(c(10,3,5), nrow = 1, ncol = 3)
y
Dot(x,y)

