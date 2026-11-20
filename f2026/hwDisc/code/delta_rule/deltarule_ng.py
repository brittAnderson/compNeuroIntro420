
'''
Navya Gulati
21054082
'''

import random

# Settings
random.seed(42)
number_training = 100
number_testing = 50
distance = 0.3
learning_rate = 0.01
epochs = 100

# Create random points
def make_points(number):
    points = []

    for i in range(number):
        x = random.uniform(-1, 1)
        desired = random.choice([-1, 1])
        gap = random.uniform(distance, distance + 0.5)
        y = 0.5 * x + 0.2 + desired * gap

        points.append((x, y, desired))

    return points

# Predict the class of a point
def predict(x, y, w1, w2, bias):
    observed = w1 * x + w2 * y + bias

    if observed >= 0:
        return 1
    else:
        return -1

# Calculate classification accuracy
def accuracy(points, w1, w2, bias):
    correct = 0

    for x, y, desired in points:
        if predict(x, y, w1, w2, bias) == desired:
            correct += 1

    return 100 * correct / len(points)

# Generate training points
training_data = make_points(number_training)

# Start with random weights
w1 = random.uniform(-1, 1)
w2 = random.uniform(-1, 1)
bias = random.uniform(-1, 1)

print("Before training:",
      accuracy(training_data, w1, w2, bias), "%")

# Train using the delta rule
for epoch in range(epochs):
    random.shuffle(training_data)

    for x, y, desired in training_data:
        observed = w1 * x + w2 * y + bias
        error = desired - observed

        w1 = w1 + x * learning_rate * error
        w2 = w2 + y * learning_rate * error
        bias = bias + 1 * learning_rate * error

# Show learned weights and training accuracy
print("Epochs completed:", epochs)
print("Learned weights:", w1, w2, bias)
print("Training accuracy:",
      accuracy(training_data, w1, w2, bias), "%")

# Generate new testing points
testing_data = make_points(number_testing)

# Test without changing the weights
print("Testing accuracy:",
      accuracy(testing_data, w1, w2, bias), "%")

# Show first five predictions
for x, y, desired in testing_data[:5]:
    print("Actual:", desired,
          "Predicted:", predict(x, y, w1, w2, bias))
