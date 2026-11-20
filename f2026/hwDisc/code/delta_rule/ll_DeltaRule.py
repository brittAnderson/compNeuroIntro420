# imports
import numpy as np


# setup
Matrix = np.array([
    [-0.6, -0.8, 1.0],
    [-0.2, -0.2, -1.0],
    [0.3, -0.7, 1.0],
    [-0.6, -0.9, 1.0],
    [0.5, -0.8, 1.0],
    [-0.8, -0.7, -1.0],
    [0.8, -0.6, 1.0],
    [0.6, -0.3, 1.0]
    ])

initW = np.array([
    [0.3],
    [-0.8]
    ]) 
W = initW

row = {} 
pos = 0
rows = Matrix.shape[0] 
tol = 0.0001
bias = 1
eta = 0.001

step = 0
maxSteps = 5000


while step < maxSteps:
    c = Matrix[pos, -1] #class
    a = Matrix[pos, 0] * W[0]
    b = Matrix[pos, 1] * W[1]
    I = (a + b) + bias
# updating the weights
    X = np.array([
        [Matrix[pos, 0]],
        [Matrix[pos, 1]]
        ])                  #inputs
    W = W + X * eta * (c - I)
    bias = bias + eta * (c - I)
# moving onto the next row
    row[step] = W
    pos += 1
    if pos >= rows:
        pos = 0
    step += 1
# stopping condition
    e = 0.5 * ((c - I) ** 2)
    if e < tol:
        break



print(f"{row}")

