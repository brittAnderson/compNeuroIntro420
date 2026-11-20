# Imports
import random
import numpy as np

# Points generation, -1 is below the line, 1 is above
def generate_points(m, b, size):
    points_x1 = []
    points_y1 = []
    points_x_minus_1 = []
    points_y_minus_1 = []
    for i in range(int(size/2)):
        # Below the line
        x_minus_1 = round(random.uniform(0, b), 2)
        y_minus_1 = round(random.uniform(0, x_minus_1 * m + b - 0.1), 2)
        # Above the line
    x1 = round(random.uniform(b, b*2), 2)
    y1 = round(random.uniform(x1 * m + b + 0.1, b*2), 2)
    points_x1.append(x1)
    points_y1.append(y1)
    points_x_minus_1.append(x_minus_1)
    points_y_minus_1.append(y_minus_1)
    point_values = np.array(list(zip(points_x_minus_1 + points_x1, points_y_minus_1 + points_y1)))
    group_values = [-1] * int(size/2) + [1] * int(size/2)
    return list(zip(point_values, group_values))

# Weight solving function using the Delta Rule
def solve_weights(points):
    # Weight initialization and learning rate
    weights = np.random.uniform(-1, 1, size=2)
    bias = np.random.uniform(-1, 1)
    learning_rate = 0.1

    # Delta Rule implementation
    while True:
        errors = 0
        for xi, desired in points:
            activation = np.dot(weights, xi) + bias
            observed = 1 if activation >= 0 else -1
            if round(observed, 2) != round(desired, 2):
                # Δw = xi * η * (desired - observed)
                weight_change = xi * learning_rate * (desired - observed)
                weights += weight_change
                # Δb = η * (desired - observed)
                bias += learning_rate * (desired - observed)
                errors += 1
        if errors == 0:
            break
    return weights, bias

# Checks accuracy of learned weights and bias against a set of points
def check_accuracy(points, weights, bias):
    correct_predictions = 0
    for xi, desired in points:
        activation = np.dot(weights, xi) + bias
        observed = 1 if activation >= 0 else -1
        if round(observed, 2) == round(desired, 2):
            correct_predictions += 1
    accuracy = correct_predictions / len(points)
    return accuracy

# Line Definition
separation_m = 0.5
separation_b = 2.0
number_of_points = 20

# Point generation
Learning_points = generate_points(separation_m, separation_b, number_of_points)
Test_points = generate_points(separation_m, separation_b, number_of_points)

# Weight solving using learning points and accuracy checking using test points
line_weights, line_bias = solve_weights(Learning_points)
test_accuracy = check_accuracy(Test_points, line_weights, line_bias)
print("Line Weights:", line_weights)
print("Line Bias:", line_bias)
print("Test Accuracy:", test_accuracy)

# Credit to Microsoft Copilot for assistance in bugfixing, as I was unaware bias was needed for lines not centered on the origin.