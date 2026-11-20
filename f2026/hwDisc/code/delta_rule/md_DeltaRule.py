import numpy as np 
import matplotlib.pyplot as plt

margin = 0.25 
n = 10

x1 = np.random.uniform(-1, 1, n)
threshold = 0.5 * x1 + 0.2

class1 = np.column_stack([x1, threshold + margin + np.random.rand(n)*0.2])
class2 = np.column_stack([x1, threshold - margin - np.random.rand(n)*0.2])

x = np.vstack([class1, class2])
y = np.hstack([np.ones(n), -np.ones(n)])

class TDR: 
    def __init__(self, eta = 0.1):
        self.eta = eta

    def fit(self, x, y, epochs=100):
        x = np.c_[x, np.ones(len(x))]
        self.weights = np.random.rand(x.shape[1])
        self.errors = []
        for _ in range(epochs):
            error_sum = 0 
            for xi, target in zip (x, y):
                output = np.dot(xi, self.weights)
                error = target - output 
                self.weights += self.eta * error *xi
                error_sum += error**2
            self.errors.append(error_sum)

    def predict (self, x): 
        x = np.c_[x, np.ones(len(x))]
        return np.dot(x, self.weights)

model = TDR(eta=0.1)
model.fit(x, y, epochs=100)

print("Weights:", model.weights)
print("Predictions:", model.predict(x))

predictions = np.where(model.predict(x) >= 0, 1, -1)
print(predictions)

plt.scatter(
    x[y == 1][:, 0],
    x[y == 1][:, 1],
    color='cyan',
    label='+1'
)

plt.scatter(
    x[y == -1][:, 0],
    x[y == -1][:, 1],
    color='magenta',
    label='-1'
)

plt.legend()
plt.axis('equal')
plt.show()