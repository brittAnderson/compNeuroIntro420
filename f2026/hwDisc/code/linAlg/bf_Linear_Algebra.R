# Linear algebra. Performing basic matrix and vector operations

A <- matrix(c(1,3,5,2,4,6), nrow = 3, ncol = 2)     #3x2
B <- matrix(c(7,9,11,8,10,12), nrow = 2, ncol = 3)  #2x3
C <- matrix(c(1,3,2,4), nrow = 2, ncol = 2)    #2x2
D <- matrix(c(5,7,6,8), nrow = 2, ncol = 2)    #2x2
column_vector <- matrix(c(3,5,7), nrow = 3, ncol = 1)
row_vector <- matrix(c(4,6,8), nrow = 1, ncol = 3)

#summation
CD_sum <- C+D
#transposing (flip x and y)
A_Transpose <- t(A)
#adding the transposed matrix to only other matrix permissible
AB_addition <- A_Transpose+B
#multiplication
AB_product <-A%*%B
#dot product (multiplies matching entries and adds the products)
dot_product <- row_vector%*%column_vector


#XOR truth table (extra)
a <- c(0, 1, 0, 1)
b <- c(0, 0, 1, 1)
or_output <- (a == 1) | (b == 1)
and_output <- (a == 1) & (b == 1) 
   nand_output <- !and_output
xor_output <- (or_output & nand_output)
as.integer(xor_output)
data.frame(a = a, b = b, XOR = as.integer(xor_output))


#XOR wiring diagram from a series of Boolean gates (extra)
plot(NA, xlim = c(0, 10), ylim = c(0, 6),
     axes = FALSE, xlab = "", ylab = "",
     main = "XOR from OR, NAND, and AND gates")
text(0.7, 4.5, "A", cex = 1.4)
text(0.7, 1.5, "B", cex = 1.4)
text(9.4, 3, "XOR", cex = 1.4)
#Gates
rect(3.5, 3.8, 5, 5.2, col = "white")
text(4.25, 4.5, "OR")
rect(3.5, 0.8, 5, 2.2, col = "white")
text(4.25, 1.5, "NAND")
rect(6.8, 2.3, 8.2, 3.7, col = "white")
text(7.5, 3, "AND")
#Arrows
arrows(1.1, 4.5, 3.5, 4.8, col = "firebrick", lwd = 2, length = 0.1)
arrows(1.1, 4.5, 3.5, 1.8, col = "firebrick", lwd = 2, length = 0.1)
arrows(1.1, 1.5, 3.5, 4.2, col = "darkorange3", lwd = 2, length = 0.1)
arrows(1.1, 1.5, 3.5, 1.2, col = "darkorange3", lwd = 2, length = 0.1)
arrows(5, 4.5, 6.8, 3.3, col = "steelblue4", lwd = 2, length = 0.1)
arrows(5, 1.5, 6.8, 2.7, col = "steelblue4", lwd = 2, length = 0.1)
arrows(8.2, 3, 8.8, 3, col = "green4", lwd = 2, length = 0.1)
