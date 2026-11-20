#!/usr/bin/env python
# coding: utf-8

# In[2]:


import numpy as np
import matplotlib.pyplot as plt

np.random.seed(42)
n_points = 40
margin = 1

X = []

while len(X) < n_points:
    point = np.random.uniform(-10, 10, 2)
    if abs(point[0] + point[1]) >= margin:
        X.append(point)

X = np.array(X)
y = np.where(X[:, 0] + X[:, 1] > 0, 1, -1)

plt.scatter(X[y == 1, 0], X[y == 1, 1], label="+1")
plt.scatter(X[y == -1, 0], X[y == -1, 1], label="-1")

x = np.linspace(-10, 10, 100)
plt.plot(x, -x, "k--", label="Correct Boundary")

plt.xlabel("x1")
plt.ylabel("x2")
plt.title("Training Data")
plt.legend()
plt.show()

weights = np.random.randn(2)
bias = 0
learning_rate = 0.001

for epoch in range(100):
    for i in range(len(X)):

        observed = np.dot(X[i], weights) + bias
        desired = y[i]

        error = desired - observed

        weights = weights + learning_rate * X[i] * error
        bias = bias + learning_rate * error

print("Final weights:", weights)
print("Final bias:", bias)

X_test = np.random.uniform(-10, 10, (100, 2))

y_test = np.where(
    X_test[:, 0] + X_test[:, 1] > 0, 1, -1)

correct = 0

for i in range(len(X_test)):
    observed = np.dot(X_test[i], weights) + bias
    if observed >= 0:
        prediction = 1
    else:
        prediction = -1

    if prediction == y_test[i]:
        correct += 1

accuracy = correct / len(X_test) * 100

print("Correct predictions:", correct)
print("Test accuracy:", accuracy, "%")

plt.scatter(X_test[y_test == 1, 0],
            X_test[y_test == 1, 1], label="+1")

plt.scatter(X_test[y_test == -1, 0],
            X_test[y_test == -1, 1], label="-1")

x = np.linspace(-10, 10, 100)

plt.plot(x, -x, "k--", label="Correct Boundary")

if abs(weights[1]) > 0.000001:
    learned_y = -(weights[0] * x + bias) / weights[1]
    plt.plot(x, learned_y, label="Learned Boundary")

plt.xlabel("x1")
plt.ylabel("x2")
plt.title("Testing the Trained Neuron")
plt.legend()
plt.show()

