""" Fourth Root
This module is written for comparison with Julia code.
"""
MAX_ITERATIONS: int = 100 # The maximum number of iterations allowed in this module.
TOLERANCE: float = 1.e-6 # The tolerance is 1x10^-6.

def fourth_root(x: float):
    """ Returns the fourth root of x. """
    return x ** (1/4)


def nth_root(x: float, n: int):
    """ Returns the nth root of x. """
    return x ** (1/n)


def fourth_root_manual(x: float, guess: float = 1):
    """ Returns the fourth root of x, using the Newton-Raphson method. """
    iterations: int = 0
    while (TOLERANCE <= abs(x - guess**4)) and (iterations < MAX_ITERATIONS):
        derivative: float = (4 * (guess**3)) # derivative of x^4 is 4x^3
        guess = guess + (x - guess**4) / derivative # update guess using Netwon Raphson
        iterations += 1 # Update the number of iterations
    return guess


def nth_root_manual(x: float, n: int, guess: float = 1):
    """ Returns the nth root of x, using the Newton Raphson method. """
    iterations: int = 0=
    while (TOLERANCE <= abs(x - guess**n)) and (iterations < MAX_ITERATIONS):
        derivative: float = n * (guess**(n-1)) # derivative of x^n is (n)x^(n-1) by the Chain Rule.
        guess = guess + (x - guess**n) / derivative # update guess using Netwon Raphson
        iterations += 1 # Update the number of iterations
    return guess


if __name__ == "__main__":
    number1: float = 36

    answer1 = fourth_root(number1)
    print(f"The fourth root of {number1} is {answer1}.")

    answer2 = nth_root(number1, 5)
    print(f"The fifth root of {number1} is {answer2}.")

    answer3 = fourth_root_manual(number1, guess=1)
    print(f"The fourth root estimation of {number1} using Newton Raphson is {answer3}.")

    answer4 = nth_root_manual(number1, 5, guess=1)
    print(f"The fifth root estimation of {number1} using Newton Raphson is {answer4}.")

