import numpy as np

# How far points neet to be from the line y = x
margin = 2

# empty lists that will collect each  point (x,y)
X = []
y = []

for i in range(100): #100 training examples

    x = np.random.randint(-10, 11) # saying that itll generate -10 to 10, the 11 is what it will never reach so if it was -10,12, the range would be -10 to 11

    # np.random.random() gives a number between 0 and 1
    if np.random.random() > 0.5: # if it's greater than 0.5 it creates a point above the line. Otherwise create a point below the line.
        yvalue = x + np.random.randint(margin, 11)
        label = 1 # is y>x
    else:
        yvalue = x - np.random.randint(margin, 11)
        label = -1 #if y<x

    X.append([x, yvalue]) # stores the point x,y
    y.append(label) #stores the lable 1 or -1

#converst lists to arrays  so instead of follwoing eahcother each point will appear bellow the next one
X = np.array(X)
y = np.array(y)

# creates two weights because there are 2 inputs aka x and y
weights = np.random.random(2)

# random starting bias
bias = np.random.random()

learningRate = 0.1

# Train the model
for epoch in range(100): # tells it to run through the training data 100 times

    for i in range(len(X)):
        point = X[i] # gets the point
        desired = y[i] #gets the right answer 
        result = np.dot(point, weights) + bias #dot product

        if result >= 0:
            observed = 1
        else:
            observed = -1

        error = desired - observed
        weights = weights + (point * learningRate * error)
        bias = bias + (learningRate * error)

# create test data
Xtest = []
ytest = []

for i in range(50): # generates 50  test points

    x = np.random.randint(-10, 11)

    if np.random.random() > 0.5:
        yvalue = x + np.random.randint(margin, 11)
        label = 1
    else:
        yvalue = x - np.random.randint(margin, 11)
        label = -1

    Xtest.append([x, yvalue])
    ytest.append(label)

Xtest = np.array(Xtest)
ytest = np.array(ytest)

# Test accuracy
correct = 0

for i in range(len(Xtest)):

    point = Xtest[i]

    actual = ytest[i]

    result = np.dot(point, weights) + bias

    if result >= 0:
        prediction = 1
    else:
        prediction = -1

    if prediction == actual:
        correct = correct + 1

accuracy = (correct / len(Xtest)) * 100

print("final Weights:", weights)

print("Bias:", bias)

print("Correct:", correct)
print("Total:", len(Xtest))
print("Accuracy:", accuracy, "%")