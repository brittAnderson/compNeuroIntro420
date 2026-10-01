# Dependencies
from scipy.integrate import solve_ivp
import numpy as np
import matplotlib.pyplot as plt

# Initial variables
t = np.linspace(0, 50, 1000) # Time array
p = 2.0  # Spring constant
x0 = 0 # Initial position
v0 = 1 # Initial velocity

# Spring DE
def equation(t, y):
    x, v = y
    return [v, -p * x]

# Solve spring DE
solution = solve_ivp(equation, [t[0], t[-1]], [x0, v0], t_eval=t)

# Plot solution
plt.plot(t, solution.y[0])
plt.title('Spring Motion Differential Equation Plot')
plt.xlabel('Time')
plt.ylabel('Position')
plt.show()
