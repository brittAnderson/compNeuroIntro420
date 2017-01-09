def cube (x):
    return x**3

def derivCube (x):
    return 3*x**2

def cubeRoot(n,initGuess = 5.0):
    guesses = [initGuess]
    errors = [10000.0]
    tolerance = 0.01
    i = 0
#    while not (errors[-1] < tolerance):
    while (abs(errors[-1]) > tolerance):
        curError = n - cube(guesses[-1])
#        print("curError = %f" % curError)
        errors.append(curError)
        newGuess = errors[-1]/derivCube(guesses[-1]) + guesses[-1]
#       print ("newGuess = %f" % newGuess)
        guesses.append(newGuess)

    return(guesses[-1])

def main():
    testNum = input("Cube root of ?\n")
    print ("Answer is: %f\n" % cubeRoot(float(testNum)))

if __name__ == "__main__":
    main()
