#Neural network
#PSYCH 420

#Imports
import random
import math
import numpy as np


# Creating a class 'Network":
class Network(object):

    # Initializer:
    def __init__(self):

        # Define Hyperparameter:
        # Number of input samples:
        # where, a is the # of rows in the input
        a = int(raw_input('Choose an input number:  ') )
        # Input layer size:
        # where, b is the # of columns in the input
        b = int(raw_input('Choose an input layer size:  ')) + 1 # add the bias
        # Hidden layer size:
        # where, n is the # of columns in the weights of synapse 1, randomized between {1, 3}
        n = random.randint(1,3)
        # Output layer size:
        # where, k is the # of columns in the weights of synapse 2, randomized between {1 ,3}
        k = random.randint(1,3)

        # Inputs & Output parameters:
        # Input Layer is randomized with the user's matrix dimensions
        self.inputs = [[random.random()*2-0.5 for cols in range(b)] for rows in range(a)]
        # Desired output data
        # if the desired output randomized is less than 0.5, than the value is 1,
        # if the desired output randomized is greater than 0.5, than the value is 0.
        self.y = [[1 if random.random() < 0.5 else 0 for cols in range(k)] for i in self.inputs]

        # Weights parameters:
        # Hidden layer weights
        self.weightsHidden = [[random.random()*0.4-0.2 for cols in range(n)] for rows in range(b)]
        # Output layer weights
        self.weightsOutput = [[random.random()*0.4-0.2 for cols in range(k)] for rows in range(n)]

        # learning  rate:
        self.learningrate = float(raw_input('Choose a learning rate between {0.1,0.5}: ') )

    # String Function
    # "\n" creates a new line
    def toString(self):
        res = "Matrix Dimensions for the Input (%s x %s) " %(len(self.inputs),len(self.inputs[0]))
        res += "\nInput Layer\n"
        for row in self.inputs:
            res += str(row) + "\n"

        res += "\nWeights for synapse 1\n"
        for row in self.weightsHidden:
            res += str(row) + "\n"

        res += "\nWeights for synapse 2\n"
        for row in self.weightsOutput:
            res += str(row) + "\n"

        res += "\n dJdW1 \n"
        res += str(self.dJdW1) + "\n"

        res += "\n dJdW2 \n"
        res += str(self.dJdW2) + "\n"

        res += "\nCost\n"
        res += str(self.cost) + "\n"

        res += "\nDesired Output\n"
        for row in self.y:
            res += str(row) + "\n"

        res += "\n Loop #\n"
        res += str(self.countIter) +"\n"

        res += "\n Observed Output \n"
        res += str(self.output) + "\n"

        return(res)

    # Sigmoid function:
    def sigmoid(self, z):
        return 1 / (1 + np.exp(-z))

    # Derivative of the sigmoid function, in terms of the output (i.e. z):
    def dsigmoid(self, z):
        return self.sigmoid(z) * (1.0 - self.sigmoid(z))

    # Hidden layer Calculations:
    # activation Function by using the sigmoid function
    def updateSynapse1(self):
        self.z2 = np.dot(self.inputs, self.weightsHidden)
        self.hiddenlayer = self.sigmoid(self.z2)

    # Output Calculations:
    # activation Function by using the sigmoid function
    def updateSynapse2(self):
        self.z3 = np.dot(self.hiddenlayer, self.weightsOutput)
        self.output = self.sigmoid(self.z3)

    # Function that computes cost:
    # set the initial cost to be zero
    # zip (the desired output and the observed output)
    def updateCost(self):
        self.cost = 0.0
        for exp,obs in zip(self.y, self.output):
            self.cost += 0.5 * (abs(exp - obs)** 2)

    # Derivative of Cost Function
    # compute derivative with respect to weights in the hidden layer and output for given inputs and output
    # dJdW2 is the partial derivative of cost with respect to the weights for the output layer
    # dJdW1 is the partial derivative of cost with respect to the weights in the hidden layer
    def costPrime(self):
        backProp_error1 = np.multiply(-(self.y - self.output), self.dsigmoid(self.z3))
        self.dJdW2 = np.dot(self.hiddenlayer.T, backProp_error1)

        backProp_error2 = np.dot(backProp_error1, (np.asarray(self.weightsOutput).T)) * self.dsigmoid(self.z2)
        self.dJdW1 = np.dot((np.asarray(self.inputs).T), backProp_error2)

    # Adjusting the weights in the layers with dJdW2 & dJdW1 & the learning rate
    def train(self):
        self.weightsOutput = self.weightsOutput - self.dJdW2 * self.learningrate
        self.weightsHidden = self.weightsHidden - self.dJdW1 * self.learningrate

    # Due to the fact that cost will not reach zero,
    # once the cost is below the tolerance,
    # the observed output will be rounded off to a 1 or a 0 to match the desired output:
    def activate(self):
        for i in xrange(len(self.output)):
            for j in xrange(len(self.output[0])):
                if self.output[i][j] > 0.8:
                    self.output[i][j] = 1
                elif self.output[i][j] < 0.2:
                    self.output[i][j] = 0
        return self.output

    # Loop function that allows the program to run until the cost is below a tolerance
    def loop(self):
        self.countIter = 0
        done = False
        while not done:
            self.updateSynapse1()
            self.updateSynapse2()
            self.updateCost()
            self.costPrime()
            self.train()

            #Terminate
            self.countIter += 1
            tolerance =0.01
            if all(i < tolerance for i in self.cost):
                done = True
                self.output = self.activate()

            print(self.toString())


# Allows to run the class:
network = Network()
network.loop()
