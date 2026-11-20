# delta rule assignment
# skeleton template, guidance, and debugging provided by Claude

import random   # random number generator
import math     # for the square root

# step 1: make labelled data
def make_data(n, margin, seed): # n = how many points, margin = min distance from line, seed = repeatable randomness
    rng = random.Random(seed)

    a, b, c = 1, 1, 1 # weights, rule that perceptron needs to learn

    points = []
    labels = []

    while len(points) < n: # keep going until we have n points
        x1 = rng.uniform(-5, 5)
        x2 = rng.uniform(-5, 5)

        dist = ((a * x1) + (b * x2) + c) / (math.sqrt((a**2) + (b**2))) # signed distance to the true line

        if (dist > 0 and dist < margin) or (dist < 0 and dist > -margin): # too close to the line?
            continue # skip it

        points.append([x1, x2])

        if dist > 0: # which side of the line -> which label
            labels.append(1)
        elif dist < 0:
            labels.append(-1)

    return points, labels

x, y = make_data(20, 0.5, seed = 1) # training data
print(y.count(1), "points labelled +1,", y.count(-1), "labelled -1")

# step 2: random starting weights
rng = random.Random(32)
w0 = rng.uniform(-1, 1) # bias weight
w1 = rng.uniform(-1, 1) # weight for x1
w2 = rng.uniform(-1, 1) # weight for x2
w = [w0, w1, w2]
print("starting weights:", w)

# step 3: train with the delta rule until all points are correct
eta = 0.1 # learning rate
max_passes = 100 # safety limit
passes = 0 # pass counter
mistakes = 1 # starts above 0 so the while loop runs at least once

while mistakes > 0 and passes < max_passes: # repeat passes until a pass has no mistakes
    mistakes = 0
    passes = passes + 1

    for i in range(len(x)): # one pass: every training point
        p1 = x[i][0]
        p2 = x[i][1]
        desired = y[i]

        net = w[0] + (w[1] * p1) + (w[2] * p2) # guess
        if net >= 0:
            observed = 1
        else:
            observed = -1

        error = desired - observed # delta rule: learn from mistakes
        if error != 0:
            mistakes = mistakes + 1
        w[0] = w[0] + eta * error * 1
        w[1] = w[1] + eta * error * p1
        w[2] = w[2] + eta * error * p2

    print("pass", passes, "mistakes:", mistakes)

print("training finished after", passes, "passes")
print("final weights:", w)

# step 4: test on new points the perceptron has not seen
testPoints = 20
x_test, y_test = make_data(testPoints, 0.5, seed = 64) # how many test points and a new seed

correct = 0 # count of right guessses
i = 0 

while i < testPoints: # loop over test points
    p1 = x_test[i][0]
    p2 = x_test[i][1]
    desired = y_test[i]

    net = w[0] + (w[1] * p1) + (w[2] * p2) # guess with trained weights
    
    if net >= 0:
        observed = 1
    else:
        observed = -1
    
    if observed == desired:
        correct += 1

    i += 1

accuracy = correct / testPoints
print("correct: ", correct, "out of ", len(x_test))
print("test accuracy: ", accuracy)