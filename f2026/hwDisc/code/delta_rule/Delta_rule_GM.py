import numpy as np

X = np.array([ #x axis points
    [1.0, 0.5, 1.0],
    [1.0, 0.2, 0.8],
    [1.0, -0.1, 0.4],
    [1.0, -0.9, 1.0]
])

y = np.array([1.0, 1.0, -1.0, -1.0]) #y axis points

w = np.array([0.0, 0.1, 0.2]) #input weights
n = 0.1 

while not np.allclose(y, X @ w, atol=0.0001): #loops delta rule until reaches certain threshold value
    for xi, des in zip(X, y):
        obs = xi @ w
        w = w + xi * n * (des - obs) #delta rule

print("weights", np.round(w, 4))

X_test = np.array([ #tested values for x
    [1.0, 0.6, 0.9],
    [1.0, -0.5, 0.6],
])

y_test = np.array([1.0, -1.0]) #tested values for y

preds = np.where((X_test @ w) >= 0, 1.0, -1.0) #weighs accuracy of new weights
print("test accuracy", np.mean(preds == y_test))