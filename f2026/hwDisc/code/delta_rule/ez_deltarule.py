import numpy as np
import matplotlib.pyplot as plt

# AI DECLARATION
# i used GPT Sol 6.1 for generate_points() and to make the plot
# prompt 1: given a,b of linear fn y=ax+b, use np to randomly generate 10 points on each side of the line
# prompt 2: read contents of attached file; amend to generate a plot of average error over runs
def generate_points(a, b):
    id = np.repeat([-1, 1], 10)
    x = np.ones((20, 3))
    x[:, 0] = np.random.uniform(-10, 10, 20)
    x[:, 1] = a * x[:, 0] + b + id * np.random.uniform(1, 5, 20)
    return x, id


def train(x, id):
    k = 0.1
    w = np.random.uniform(-1, 1, 3)
    runs = 0
    average_error = 1
    errors = []

    while not average_error == 0:
        runs += 1
        for i in range(len(x)):
            y = 1 if np.dot(x[i], w) >= 0 else -1
            B = 1 if y == id[i] else -1
            w += k * x[i] * (id[i] - y)

        y = np.where(np.dot(x, w) >= 0, 1, -1)
        average_error = np.mean(np.abs(id - y))
        errors.append(average_error)

    return w, runs, errors


def main():
    print("Target line: y = a*x + b")
    a = float(input("Enter a: "))
    b = float(input("Enter b: "))
    x, id = generate_points(a, b)
    w, runs, errors = train(x, id)
    print(f"Final weights w = {w.tolist()}")
    print(f"Number of runs: {runs}")

    plt.plot(range(1, runs + 1), errors)
    plt.xlabel("Run")
    plt.ylabel("Average absolute error")
    plt.title("Average error over runs")
    plt.tight_layout()
    plt.show()


if __name__ == "__main__":
    main()
