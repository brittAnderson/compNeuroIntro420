
##AI disclaimer, I wass struggling on figuring out how to get python to run the delta rule, so I used a reference to help me. Specifically with the epochs
import random

training_data = []

for i in range(50):
    x = random.uniform(0, 10)
    y = random.uniform(0, 10)

    if y > x:
        target = 1
    else:
        target = -1

    training_data.append((x, y, target))

w1 = random.uniform(-1, 1)
w2 = random.uniform(-1, 1)
bias = random.uniform(-1, 1)

learning_rate = 0.01
epochs = 100

for epoch in range(epochs):

    for x, y, target in training_data:

        # neuron output
        output = (w1 * x) + (w2 * y) + bias

        # delta rule
        error = target - output

        w1 = w1 + (learning_rate * error * x)
        w2 = w2 + (learning_rate * error * y)
        bias = bias + (learning_rate * error)

test_data = []

for i in range(20):
    x = random.uniform(0, 10)
    y = random.uniform(0, 10)

    if y > x:
        target = 1
    else:
        target = -1

    test_data.append((x, y, target))

correct = 0

for x, y, target in test_data:

    output = (w1 * x) + (w2 * y) + bias

    if output >= 0:
        prediction = 1
    else:
        prediction = -1

    if prediction == target:
        correct += 1

accuracy = (correct / len(test_data)) * 100

print("Final Weights:")
print("w1 =", round(w1, 3))
print("w2 =", round(w2, 3))
print("bias =", round(bias, 3))

print("\nCorrect Predictions:", correct)
print("Total Test Points:", len(test_data))
print("Accuracy:", round(accuracy, 2), "%")