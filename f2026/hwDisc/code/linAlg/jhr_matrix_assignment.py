#!/usr/bin/env python
# coding: utf-8

import numpy as np

A = np.array([[1, 2], [3, 4]])
B = np.array([[5, 6], [7, 8]])
C = np.array([[1, 2], [3, 4], [5, 6]])
D = np.array([[7, 8, 9], [10, 11, 12]])


suma_b = A + B
print(suma_b)

C_transpose = C.T
sumc_d = C_transpose + D
print(sumc_d)

product3x3 = C @ D
print(product3x3)

column_vector = np.array([[1], [2], [3]])
row_vector = np.array([[4, 5, 6]])
dot_product = np.dot(row_vector, column_vector)
print(dot_product)


